module Test.Serialization where

import Test.Serialization.Types
  ( TestSuiteM, TestSuiteState, emptyTestSuiteState, TestTopic, TestTopicState (..)
  , ClientToServer (..), ServerToClient (..), ChannelMsg (..)
  , isOkay, HasTopic (..), GenValue (..), HasServerS (..), DesValue (..)
  , HasServerG (..)
  , generateValue, gotServerGenValue, gotServerSerialize, gotServerDeSerialize
  , deserializeValueServerOrigin, serializeValueServerOrigin, verify
  , getTopicState
  )

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.URI (URI (..), Scheme (..), HierarchicalPart (..), Authority)
import Data.URI.URI as URI
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Argonaut (encodeJson)
import Data.Foldable (for_)
import Data.UUID (GENUUID, genUUID)
import Data.Exists (runExists)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Aff (Aff, runAff_, forkAff, Fiber, killFiber)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, warn)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef, modifyRef)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, throw, error)
import Control.Monad.Eff.Random (RANDOM)
import Node.Buffer (fromString) as Buffer
import Node.Encoding (Encoding (UTF8)) as Buffer
import ZeroMQ
  ( ZEROMQ, socket, router, dealer, connect, sendJson, readJson, setOption, zmqIdentity
  , close, Socket, Router, Dealer, Connected)
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

  topicsPendingRef <- newRef topics

  let resolve eX = case eX of
        Left e -> throwException e
        Right _ -> pure unit

  runAff_ resolve $ do
    liftEff $ send client GetTopics
    mX <- readJson client
    case mX of
      Nothing -> liftEff $ throw "No receipt"
      Just {msg} -> case msg of
        TopicsAvailable ts
          | ts == topics -> do
            -- spark a listening async thread
            receiver <- forkAff (receiveClient suiteStateRef client topicsPendingRef)
            -- initiate tests
            for_ ts \t -> do
              mX' <- liftEff $ generateValue suiteStateRef t
              case mX' of
                HasTopic mX'' -> case mX'' of
                  GenValue outgoing ->
                    liftEff $ send client $ ClientToServer outgoing
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
              -> Ref (Set TestTopic)
              -> Aff (Effects eff) Unit
receiveClient suiteStateRef client topicsPendingRef = forever $ do
  mX <- readJson client
  case mX of
    Nothing -> pure unit -- liftEff $ throw "No value?"
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
                  liftEff $ send client (ClientToServer outgoing)
                _ -> liftEff $ do
                  dumpTopic suiteStateRef t
                  throw $ "Bad deserialize value: " <> show t <> ", " <> show mOutgoing
            else liftEff $ do
              dumpTopic suiteStateRef t
              throw $ "Bad got serialize: " <> show t <> ", " <> show mOk
        GeneratedInput t x -> do
          mOk <- liftEff $ gotServerGenValue suiteStateRef t x
          if isOkay mOk
            then do
              mOutgoing <- liftEff $ serializeValueServerOrigin suiteStateRef t
              case mOutgoing of
                HasTopic (HasServerG outgoing) ->
                  liftEff $ send client (ClientToServer outgoing)
                _ -> liftEff $ do
                  dumpTopic suiteStateRef t
                  throw $ "Bad serialize value: " <> show t <> ", " <> show mOutgoing
            else liftEff $ do
              dumpTopic suiteStateRef t
              throw $ "Bad got gen: " <> show t <> ", " <> show mOk
        DeSerialized t x -> do
          mOk <- liftEff $ gotServerDeSerialize suiteStateRef t x
          if isOkay mOk
            then do
              mOk' <- liftEff $ verify suiteStateRef t
              if isOkay mOk'
                then pure unit -- FIXME silent success?
                else liftEff $ do
                  dumpTopic suiteStateRef t
                  throw $ "Bad verify: " <> show t <> ", " <> show mOk'
            else liftEff $ do
              dumpTopic suiteStateRef t
              throw $ "Bad got deserialize: " <> show t <> ", " <> show mOk
      Continue t -> do
        mX' <- liftEff $ generateValue suiteStateRef t
        case mX' of
          HasTopic mX'' -> case mX'' of
            GenValue outgoing ->
              liftEff $ send client (ClientToServer outgoing)
            DoneGenerating -> do
              liftEff $ send client (Finished t)
              liftEff $ log $ "Topic finished: " <> show t
              liftEff $ modifyRef topicsPendingRef (Set.delete t)
              ts <- liftEff $ readRef topicsPendingRef
              when (ts == Set.empty) $ do
                liftEff $ log "Tests finished."
                liftEff $ close client
          _ -> liftEff $ throw $ "No topic for generating -- in continue: " <> show t


send :: forall eff
      . Socket Dealer Router Connected
     -> ClientToServer
     -> Eff (zeromq :: ZEROMQ, buffer :: BUFFER | eff) Unit
send client x = sendJson unit client x


dumpTopic :: forall eff
           . TestSuiteState
          -> TestTopic
          -> Eff (ref :: REF, console :: CONSOLE | eff) Unit
dumpTopic xsRef t = do
  mState <- getTopicState xsRef t
  case mState of
    NoTopic -> warn "No topic..?"
    HasTopic ex ->
      let go :: forall a. TestTopicState a -> Eff (ref :: REF, console :: CONSOLE | eff) Unit
          go (TestTopicState {serialize,clientG,serverS,clientD,serverG,clientS,serverD}) = do
            mClientG <- readRef clientG
            log $ "clientG: " <> show (serialize <$> mClientG)
            mServerS <- readRef serverS
            log $ "serverS: " <> show mServerS
            mClientD <- readRef clientD
            log $ "clientD: " <> show (serialize <$> mClientD)
            mServerG <- readRef serverG
            log $ "serverG: " <> show (serialize <$> mServerG)
            mClientS <- readRef clientS
            log $ "clientS: " <> show mClientS
            mServerD <- readRef serverD
            log $ "serverD: " <> show (serialize <$> mServerD)
      in  runExists go ex
