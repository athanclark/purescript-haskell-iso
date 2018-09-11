module Test.Serialization where

import Test.Serialization.Types
  ( TestSuiteM, TestSuiteState, emptyTestSuiteState, TestTopic, TestTopicState (..)
  , ClientToServer (..), ServerToClient (..), MsgType (..)
  , isOkay, HasTopic (..), GenValue (..), HasServerS (..), DesValue (..)
  , HasServerG (..), getTopicState
  , generateValue, gotServerGenValue, gotServerSerialize, gotServerDeSerialize
  , deserializeValueServerOrigin, serializeValueServerOrigin, verify
  , toUtf8String
  )

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.URI (URI (..), Scheme (..), HierarchicalPart (..), Authority)
import Data.URI.URI as URI
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Argonaut (encodeJson, decodeJson, jsonParser)
import Data.Foldable (for_)
import Data.UUID (GENUUID, genUUID)
import Data.Exists (Exists, runExists)
import Data.NonEmpty (NonEmpty (..))
import Data.Generic (gShow)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Aff (Aff, runAff_, forkAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, warn)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef, modifyRef)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, throw)
import Control.Monad.Eff.Random (RANDOM)
import ZeroMQ
  ( ZEROMQ, socket, router, dealer, connect, sendMany, readMany, readJson'
  , setOption, zmqIdentity, close, Socket, Router, Dealer, Connected)
import Node.Buffer (BUFFER, Buffer)
import Node.Buffer (fromString, toString) as Buffer
import Node.Encoding (Encoding (UTF8)) as Buffer




type ClientParams (eff :: # Effect) =
  { controlHost :: Authority
  , testSuite :: TestSuiteM eff Unit
  , maxSize :: Int
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
startClient {controlHost,testSuite,maxSize} = do
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
    _ <- liftEff $ send client GetTopics
    mX <- readJson' client
    case mX of
      Nothing -> liftEff $ throw "No receipt"
      Just eX -> case eX of
        Left e -> liftEff $ throw $ "JSON parse failure on init receipt: " <> e
        Right {msg} -> case msg of
          TopicsAvailable ts
            | ts == topics -> do
              -- spark a listening async thread
              receiver <- forkAff (receiveClient suiteStateRef client topicsPendingRef maxSize)
              -- initiate tests
              for_ ts \t -> do
                mState <- liftEff $ getTopicState suiteStateRef t
                case mState of
                  NoTopic -> liftEff $ throw "No topic"
                  HasTopic state -> do
                    mX' <- liftEff $ generateValue state t maxSize
                    case mX' of
                      GenValue outgoing -> do
                        o' <- liftEff $ send client outgoing
                        runExists (\(TestTopicState {clientGSent}) -> liftEff $ writeRef clientGSent (Just o')) state
                      DoneGenerating -> liftEff $ throw "Done generating on init?"
              pure unit
            | otherwise -> liftEff $ throw $ "Mismatched topics: "
                        <> show ts <> ", on client: " <> show topics
          _ -> liftEff $ throw $ "Incorrect timing?: " <> show (encodeJson msg)


receiveClient :: forall eff
               . TestSuiteState
              -> Socket Dealer Router Connected
              -> Ref (Set TestTopic)
              -> Int
              -> Aff (Effects eff) Unit
receiveClient suiteStateRef client topicsPendingRef maxSize = forever $ do
  mX <- readMany client
  case mX of
    Nothing -> pure unit -- liftEff $ throw "No value?"
    Just {msg: NonEmpty incoming _} -> do
      s <- liftEff $ Buffer.toString Buffer.UTF8 incoming
      case decodeJson =<< jsonParser s of
        Left e -> liftEff $ do
          ts <- readRef topicsPendingRef
          for_ ts \t -> dumpTopic suiteStateRef t
          _ <- send client $ ClientToServerBadParse e
          throw $ "Bad parse: " <> e
        Right msg -> case msg of
          TopicsAvailable _ -> liftEff $ throw "Re-Sent topics available?"
          ServerToClientBadParse e -> liftEff $ throw $ "Bad parse: " <> e
          ServerToClient t m y -> do
            mState <- liftEff $ getTopicState suiteStateRef t
            case mState of
              NoTopic -> liftEff $ throw "No topic"
              HasTopic state -> case m of
                Failure -> liftEff $ fail' suiteStateRef client "Failure: " t y
                -- order:
                -- clientG
                -- serverS
                -- clientD
                -- serverG
                -- clientS
                -- serverD
                -- verify
                Serialized -> do
                  runExists (\(TestTopicState {serverSReceived}) -> liftEff $ writeRef serverSReceived (Just incoming)) state
                  mOk <- liftEff $ gotServerSerialize state y
                  if isOkay mOk
                    then do
                      mOutgoing <- liftEff $ deserializeValueServerOrigin state t
                      case mOutgoing of
                        HasServerS (DesValue outgoing) -> do
                          o' <- liftEff $ send client outgoing
                          runExists (\(TestTopicState {clientDSent}) -> liftEff $ writeRef clientDSent (Just o')) state
                        _ -> liftEff $ fail' suiteStateRef client "Bad deserialize value: " t mOutgoing
                    else liftEff $ fail' suiteStateRef client "Bad got serialize: " t mOk
                GeneratedInput -> do
                  runExists (\(TestTopicState {serverGReceived}) -> liftEff $ writeRef serverGReceived (Just incoming)) state
                  mOk <- liftEff $ gotServerGenValue state y
                  if isOkay mOk
                    then do
                      mOutgoing <- liftEff $ serializeValueServerOrigin state t
                      case mOutgoing of
                        HasServerG outgoing -> do
                          o' <- liftEff $ send client outgoing
                          runExists (\(TestTopicState {clientSSent}) -> liftEff $ writeRef clientSSent (Just o')) state
                        _ -> liftEff $ fail' suiteStateRef client "Bad serialize value: " t mOutgoing
                    else liftEff $ fail' suiteStateRef client "Bad got gen: " t mOk
                DeSerialized -> do
                  runExists (\(TestTopicState {serverDReceived}) -> liftEff $ writeRef serverDReceived (Just incoming)) state
                  mOk <- liftEff $ gotServerDeSerialize state y
                  if isOkay mOk
                    then do
                      mOk' <- liftEff $ verify state
                      if isOkay mOk'
                        then liftEff $ clearState state -- clear refs on success
                        else liftEff $ fail' suiteStateRef client "Bad verify: " t mOk'
                    else liftEff $ fail' suiteStateRef client "Bad got deserialize: " t mOk
          Continue t -> do
            mState <- liftEff $ getTopicState suiteStateRef t
            case mState of
              NoTopic -> liftEff $ throw "No topic"
              HasTopic state -> do
                mX' <- liftEff $ generateValue state t maxSize
                case mX' of
                  GenValue outgoing -> do
                    o' <- liftEff $ send client outgoing
                    runExists (\(TestTopicState {clientGSent}) -> liftEff $ writeRef clientGSent (Just o')) state
                  DoneGenerating -> do
                    _ <- liftEff $ send client (Finished t)
                    liftEff $ log $ "Topic finished: " <> show t
                    liftEff $ modifyRef topicsPendingRef (Set.delete t)
                    ts <- liftEff $ readRef topicsPendingRef
                    when (ts == Set.empty) $ do
                      liftEff $ log "Tests finished."
                      liftEff $ close client


-- | Per round
clearState :: forall eff
            . Exists TestTopicState
           -> Eff (ref :: REF | eff) Unit
clearState ex = runExists go ex
  where
    go :: forall a. TestTopicState a -> Eff (ref :: REF | eff) Unit
    go (TestTopicState
      { clientG
      , clientGSent
      , serverS
      , serverSReceived
      , clientD
      , clientDSent
      , serverG
      , serverGReceived
      , clientS
      , clientSSent
      , serverD
      , serverDReceived
      }) = do
      writeRef clientG Nothing
      writeRef clientGSent Nothing
      writeRef clientS Nothing
      writeRef clientSSent Nothing
      writeRef clientD Nothing
      writeRef clientDSent Nothing
      writeRef serverG Nothing
      writeRef serverGReceived Nothing
      writeRef serverS Nothing
      writeRef serverSReceived Nothing
      writeRef serverD Nothing
      writeRef serverDReceived Nothing


fail' :: forall a eff
       . Show a
      => TestSuiteState
      -> Socket Dealer Router Connected
      -> String
      -> TestTopic
      -> a
      -> Eff ( ref :: REF
             , zeromq :: ZEROMQ
             , exception :: EXCEPTION
             , buffer :: BUFFER
             , console :: CONSOLE
             | eff) Unit
fail' suiteStateRef client prefix t v = do
  dumpTopic suiteStateRef t
  _ <- send client $ ClientToServer t Failure $ encodeJson $ show v
  throw $ prefix <> show t <> ", " <> show v
  


send :: forall eff
      . Socket Dealer Router Connected
     -> ClientToServer
     -> Eff (zeromq :: ZEROMQ, buffer :: BUFFER | eff) Buffer
send client x = do
  buf <- Buffer.fromString (show (encodeJson x)) Buffer.UTF8
  sendMany unit client (NonEmpty buf [])
  pure buf


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
          go (TestTopicState
            { serialize
            , size
            , clientG
            , clientGSent
            , serverS
            , serverSReceived
            , clientD
            , clientDSent
            , serverG
            , serverGReceived
            , clientS
            , clientSSent
            , serverD
            , serverDReceived}) = do
            size' <- readRef size
            log $ "size: " <> show size'
            mClientG <- readRef clientG
            log $ "clientG: " <> show (serialize <$> mClientG)
            showBuffer clientGSent     "  sent:     "
            mServerS <- readRef serverS
            log $ "serverS: " <> show mServerS
            showBuffer serverSReceived "  received: "
            mClientD <- readRef clientD
            log $ "clientD: " <> show (serialize <$> mClientD)
            showBuffer clientDSent     "  sent:     "
            mServerG <- readRef serverG
            log $ "serverG: " <> show (serialize <$> mServerG)
            showBuffer serverGReceived "  received: "
            mClientS <- readRef clientS
            log $ "clientS: " <> show mClientS
            showBuffer clientSSent     "  sent:     "
            mServerD <- readRef serverD
            log $ "serverD: " <> show (serialize <$> mServerD)
            showBuffer serverDReceived "  received: "
      in  runExists go ex
  where
    showBuffer bufRef prefix = do
      mBuf <- readRef bufRef
      log $ prefix <> gShow mBuf
      log $ prefix <> show (toUtf8String <$> mBuf)
