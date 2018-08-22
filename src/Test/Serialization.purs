module Test.Serialization where

import Test.Serialization.Types
  ( TestSuiteM, TestSuiteState, emptyTestSuiteState
  , ClientToServer (..), ServerToClient (..), ChannelMsg (..)
  , isOkay, HasTopic (..), GenValue (..), HasServerS (..), DesValue (..)
  , HasServerG (..)
  , generateValue, gotServerGenValue, gotServerSerialize, gotServerDeSerialize
  , deserializeValueServerOrigin, serializeValueServerOrigin, verify
  )

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.URI (URI (..), Scheme (..), HierarchicalPart (..), Authority)
import Data.URI.URI as URI
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Data.Argonaut (encodeJson)
import Data.Foldable (for_)
import Data.UUID (GENUUID, genUUID)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Aff (Aff, runAff_, forkAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, throw)
import Control.Monad.Eff.Random (RANDOM)
import Node.Buffer (fromString) as Buffer
import Node.Encoding (Encoding (UTF8)) as Buffer
import ZeroMQ
  ( ZEROMQ, socket, router, dealer, connect, sendJson, readJson, setOption, zmqIdentity
  , Socket, Router, Dealer, Connected)
import Node.Buffer (BUFFER)




type ClientParams (eff :: # Effect) =
  { controlHost :: Authority
  , testSuite :: TestSuiteM eff Unit
  }


type Effects eff =
  ( ref :: REF
  , zeromq :: ZEROMQ
  , exception :: EXCEPTION
  , buffer :: BUFFER
  , random :: RANDOM
  , console :: CONSOLE
  , uuid :: GENUUID
  | eff)


startClient :: forall eff
             . ClientParams (Effects eff)
            -> Eff (Effects eff) Unit
startClient {controlHost,testSuite} = do
  client <- socket dealer router
  connect client $ URI.print $
    URI
      (Just (Scheme "tcp"))
      (HierarchicalPart (Just controlHost) Nothing) Nothing Nothing
  ident <- (\x -> Buffer.fromString (show x) Buffer.UTF8) =<< genUUID
  setOption client zmqIdentity ident

  suiteStateRef <- emptyTestSuiteState
  topics <- do
    runReaderT testSuite suiteStateRef
    Set.fromFoldable <<< Map.keys <$> readRef suiteStateRef

  let resolve eX = case eX of
        Left e -> throwException e
        Right _ -> pure unit

  runAff_ resolve $ do
    liftEff $ sendJson unit client GetTopics
    mX <- readJson client
    case mX of
      Nothing -> liftEff $ throw "No receipt"
      Just {msg} -> case msg of
        TopicsAvailable ts
          | Set.fromFoldable ts == topics -> do
            -- spark a listening async thread
            receiver <- forkAff (receiveClient suiteStateRef client)
            for_ ts \t -> do
              mX' <- liftEff $ generateValue suiteStateRef t
              case mX' of
                HasTopic mX'' -> case mX'' of
                  GenValue outgoing ->
                    liftEff $ sendJson unit client outgoing
                  DoneGenerating -> liftEff $ throw "Done generating on init?"
                _ -> liftEff $ throw $ "Can't generate initial value? "
                       <> show t -- <> ", " <> show mX'
            pure unit
          | otherwise -> liftEff $ throw $ "Mismatched topics: "
                      <> show ts <> ", on client: " <> show topics
        _ -> liftEff $ throw $ "Incorrect timing?: " <> show (encodeJson msg)


receiveClient :: forall eff
               . TestSuiteState
              -> Socket Dealer Router Connected
              -> Aff (Effects eff) Unit
receiveClient suiteStateRef client = forever $ do
  mX <- readJson client
  case mX of
    Nothing -> liftEff $ throw "No value?"
    Just {msg} -> case msg of
      TopicsAvailable _ -> liftEff $ throw "Re-Sent topics available?"
      ServerToClientBadParse e -> liftEff $ throw $ "Bad parse: " <> e
      ServerToClient msg' -> case msg' of
        Failure t x -> liftEff $ throw $ "Topic failed: " <> show t <> ", " <> show x
        -- order:
        -- clientG
        -- serverS
        -- clientD
        -- serverG
        -- clientS
        -- serverD
        -- verify
        Serialized t x -> do
          mOk <- liftEff $ gotServerSerialize suiteStateRef t x
          if isOkay mOk
            then do
              mOutgoing <- liftEff $ deserializeValueServerOrigin suiteStateRef t
              case mOutgoing of
                HasTopic (HasServerS (DesValue outgoing)) ->
                  liftEff $ sendJson unit client outgoing
                _ -> liftEff $ throw $ show mOutgoing
            else liftEff $ throw $ show mOk
        GeneratedInput t x -> do
          mOk <- liftEff $ gotServerGenValue suiteStateRef t x
          if isOkay mOk
            then do
              mOutgoing <- liftEff $ serializeValueServerOrigin suiteStateRef t
              case mOutgoing of
                HasTopic (HasServerG outgoing) ->
                  liftEff $ sendJson unit client outgoing
                _ -> liftEff $ throw $ show mOutgoing
            else liftEff $ throw $ show mOk
        DeSerialized t x -> do
          mOk <- liftEff $ gotServerDeSerialize suiteStateRef t x
          if isOkay mOk
            then do
              mOk' <- liftEff $ verify suiteStateRef t
              if isOkay mOk'
                then pure unit -- FIXME silent success?
                else liftEff $ throw $ show mOk'
            else liftEff $ throw $ show mOk
      Continue t -> do
        mX' <- liftEff $ generateValue suiteStateRef t
        case mX' of
          HasTopic mX'' -> case mX'' of
            GenValue outgoing ->
              liftEff $ sendJson unit client outgoing
            DoneGenerating -> do
              liftEff $ sendJson unit client (Finished t)
              liftEff $ log $ "Topic finished: " <> show t
          _ -> liftEff $ throw $ "No topic for generating -- in continue: " <> show t
