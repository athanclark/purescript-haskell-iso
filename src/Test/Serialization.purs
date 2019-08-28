module Test.Serialization where

import Test.Serialization.Types
  ( TestSuiteM, TestSuiteState, emptyTestSuiteState, TestTopic, TestTopicState (..)
  , ClientToServer (..), ServerToClient (..), MsgType (..)
  , isOkay, HasTopic (..), GenValue (..), HasServerS (..), DesValue (..)
  , HasServerG (..), getTopicState
  , generateValue, gotServerGenValue, gotServerSerialize, gotServerDeSerialize
  , deserializeValueServerOrigin, serializeValueServerOrigin, verify
  , toUtf8String, ShowableJson (..), ShowableBuffer (..)
  )

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import URI
  ( AbsoluteURI (..), HierarchicalPart (HierarchicalPartAuth), Authority
  , UserInfo, Host, Path (..), Query, HierPath, Port)
import URI.HostPortPair (HostPortPair)
import URI.HostPortPair as HostPortPair
import URI.AbsoluteURI as AbsoluteURI
import URI.Scheme as Scheme
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Argonaut (encodeJson, decodeJson, jsonParser)
import Data.Foldable (for_)
import Data.UUID (genUUID)
import Data.Exists (Exists, runExists)
import Data.NonEmpty (NonEmpty (..))
import Control.Monad.Reader (runReaderT)
import Control.Monad.Rec.Class (forever)
import Effect (Effect)
import Effect.Aff (Aff, runAff_, forkAff)
import Effect.Class (liftEffect)
import Effect.Console (log, warn)
import Effect.Exception (throwException, throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import ZeroMQ
  ( socket, router, dealer, connect, sendMany, readMany, readJson'
  , setOption, zmqIdentity, close, Socket, Router, Dealer, Connected)
import Node.Buffer (Buffer)
import Node.Buffer (fromString, toString) as Buffer
import Node.Encoding (Encoding (UTF8)) as Buffer




type ClientParams =
  { controlHost :: Authority UserInfo (HostPortPair Host Port)
  , testSuite :: TestSuiteM Unit
  , maxSize :: Int
  }



startClient :: ClientParams -> Effect Unit
startClient {controlHost,testSuite,maxSize} = do
  client <- socket dealer router
  let opts =
        { printUserInfo: identity
        , printHosts: HostPortPair.print identity identity
        , printPath: identity
        , printHierPath: identity
        , printQuery: identity
        }
  connect client $ AbsoluteURI.print opts $
    let uri :: AbsoluteURI UserInfo (HostPortPair Host Port) Path HierPath Query
        uri =
          AbsoluteURI
            (Scheme.unsafeFromString "tcp")
            (HierarchicalPartAuth controlHost (Path [])) Nothing
    in  uri
  ident <- genUUID >>= \x -> Buffer.fromString (show x) Buffer.UTF8
  setOption client zmqIdentity ident

  suiteStateRef <- emptyTestSuiteState
  -- get all topics
  topics <- do
    runReaderT testSuite suiteStateRef
    Set.fromFoldable <<< Map.keys <$> Ref.read suiteStateRef

  topicsPendingRef <- Ref.new topics

  let resolve eX = case eX of
        Left e -> throwException e
        Right _ -> pure unit -- TODO close connection

  runAff_ resolve $ do
    _ <- liftEffect $ send client GetTopics
    mX <- readJson' client
    case mX of
      Nothing -> liftEffect $ throw "No receipt"
      Just eX -> case eX of
        Left e -> liftEffect $ throw $ "JSON parse failure on init receipt: " <> e
        Right {msg} -> case msg of
          TopicsAvailable ts
            | ts == topics -> do
              -- spark a listening async thread
              receiver <- forkAff (receiveClient suiteStateRef client topicsPendingRef maxSize)
              -- initiate tests
              for_ ts \t -> do
                mState <- liftEffect $ getTopicState suiteStateRef t
                case mState of
                  NoTopic -> liftEffect $ throw "No topic"
                  HasTopic state -> do
                    mX' <- liftEffect $ generateValue state t maxSize
                    case mX' of
                      GenValue outgoing -> do
                        o' <- liftEffect $ send client outgoing
                        runExists (\(TestTopicState {clientGSent}) ->
                          liftEffect $ Ref.write (Just (ShowableBuffer o')) clientGSent) state
                      DoneGenerating -> liftEffect $ throw "Done generating on init?"
              pure unit
            | otherwise -> liftEffect $ throw $ "Mismatched topics: "
                        <> show ts <> ", on client: " <> show topics
          _ -> liftEffect $ throw $ "Incorrect timing?: " <> show (ShowableJson $ encodeJson msg)


receiveClient :: TestSuiteState
              -> Socket Dealer Router Connected
              -> Ref (Set TestTopic)
              -> Int
              -> Aff Unit
receiveClient suiteStateRef client topicsPendingRef maxSize = forever $ do
  mX <- readMany client
  case mX of
    Nothing -> pure unit -- liftEffect $ throw "No value?"
    Just {msg: NonEmpty incoming _} -> do
      s <- liftEffect $ Buffer.toString Buffer.UTF8 incoming
      case decodeJson =<< jsonParser s of
        Left e -> liftEffect $ do
          ts <- Ref.read topicsPendingRef
          for_ ts \t -> dumpTopic suiteStateRef t
          _ <- send client $ ClientToServerBadParse e
          throw $ "Bad parse: " <> e
        Right msg -> case msg of
          TopicsAvailable _ -> liftEffect $ throw "Re-Sent topics available?"
          ServerToClientBadParse e -> liftEffect $ throw $ "Bad parse: " <> e
          ServerToClient t m (ShowableJson y) -> do
            mState <- liftEffect $ getTopicState suiteStateRef t
            case mState of
              NoTopic -> liftEffect $ throw "No topic"
              HasTopic state -> case m of
                Failure -> liftEffect $ fail' suiteStateRef client "Failure: " t (ShowableJson y)
                -- order:
                -- clientG
                -- serverS
                -- clientD
                -- serverG
                -- clientS
                -- serverD
                -- verify
                Serialized -> do
                  runExists (\(TestTopicState {serverSReceived}) ->
                    liftEffect $ Ref.write (Just (ShowableBuffer incoming)) serverSReceived) state
                  mOk <- liftEffect $ gotServerSerialize state y
                  if isOkay mOk
                    then do
                      mOutgoing <- liftEffect $ deserializeValueServerOrigin state t
                      case mOutgoing of
                        HasServerS (DesValue outgoing) -> do
                          o' <- liftEffect $ send client outgoing
                          runExists (\(TestTopicState {clientDSent}) ->
                            liftEffect $ Ref.write (Just (ShowableBuffer o')) clientDSent) state
                        _ -> liftEffect $ fail' suiteStateRef client "Bad deserialize value: " t mOutgoing
                    else liftEffect $ fail' suiteStateRef client "Bad got serialize: " t mOk
                GeneratedInput -> do
                  runExists (\(TestTopicState {serverGReceived}) ->
                    liftEffect $ Ref.write (Just (ShowableBuffer incoming)) serverGReceived) state
                  mOk <- liftEffect $ gotServerGenValue state y
                  if isOkay mOk
                    then do
                      mOutgoing <- liftEffect $ serializeValueServerOrigin state t
                      case mOutgoing of
                        HasServerG outgoing -> do
                          o' <- liftEffect $ send client outgoing
                          runExists (\(TestTopicState {clientSSent}) ->
                            liftEffect $ Ref.write (Just (ShowableBuffer o')) clientSSent) state
                        _ -> liftEffect $ fail' suiteStateRef client "Bad serialize value: " t mOutgoing
                    else liftEffect $ fail' suiteStateRef client "Bad got gen: " t mOk
                DeSerialized -> do
                  runExists (\(TestTopicState {serverDReceived}) ->
                    liftEffect $ Ref.write (Just (ShowableBuffer incoming)) serverDReceived) state
                  mOk <- liftEffect $ gotServerDeSerialize state y
                  if isOkay mOk
                    then do
                      mOk' <- liftEffect $ verify state
                      if isOkay mOk'
                        then liftEffect $ clearState state -- clear refs on success
                        else liftEffect $ fail' suiteStateRef client "Bad verify: " t mOk'
                    else liftEffect $ fail' suiteStateRef client "Bad got deserialize: " t mOk
          Continue t -> do
            mState <- liftEffect $ getTopicState suiteStateRef t
            case mState of
              NoTopic -> liftEffect $ throw "No topic"
              HasTopic state -> do
                mX' <- liftEffect $ generateValue state t maxSize
                case mX' of
                  GenValue outgoing -> do
                    o' <- liftEffect $ send client outgoing
                    runExists (\(TestTopicState {clientGSent}) ->
                      liftEffect $ Ref.write (Just (ShowableBuffer o')) clientGSent) state
                  DoneGenerating -> do
                    _ <- liftEffect $ send client (Finished t)
                    liftEffect $ log $ "Topic finished: " <> show t
                    void $ liftEffect $ Ref.modify (Set.delete t) topicsPendingRef
                    ts <- liftEffect $ Ref.read topicsPendingRef
                    when (ts == Set.empty) $ do
                      liftEffect $ log "Tests finished."
                      liftEffect $ close client


-- | Per round
clearState :: Exists TestTopicState
           -> Effect Unit
clearState ex = runExists go ex
  where
    go :: forall a. TestTopicState a -> Effect Unit
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
      Ref.write Nothing clientG
      Ref.write Nothing clientGSent
      Ref.write Nothing clientS
      Ref.write Nothing clientSSent
      Ref.write Nothing clientD
      Ref.write Nothing clientDSent
      Ref.write Nothing serverG
      Ref.write Nothing serverGReceived
      Ref.write Nothing serverS
      Ref.write Nothing serverSReceived
      Ref.write Nothing serverD
      Ref.write Nothing serverDReceived


fail' :: forall a
       . Show a
      => TestSuiteState
      -> Socket Dealer Router Connected
      -> String
      -> TestTopic
      -> a
      -> Effect Unit
fail' suiteStateRef client prefix t v = do
  dumpTopic suiteStateRef t
  _ <- send client $ ClientToServer t Failure $ ShowableJson $ encodeJson $ show v
  throw $ prefix <> show t <> ", " <> show v


send :: Socket Dealer Router Connected
     -> ClientToServer
     -> Effect Buffer
send client x = do
  buf <- Buffer.fromString (show $ ShowableJson $ encodeJson x) Buffer.UTF8
  sendMany unit client (NonEmpty buf [])
  pure buf


dumpTopic :: TestSuiteState
          -> TestTopic
          -> Effect Unit
dumpTopic xsRef t = do
  mState <- getTopicState xsRef t
  case mState of
    NoTopic -> warn "No topic..?"
    HasTopic ex ->
      let go :: forall a. TestTopicState a -> Effect Unit
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
            size' <- Ref.read size
            log $ "size: " <> show size'
            mClientG <- Ref.read clientG
            log $ "clientG: " <> show (ShowableJson <<< serialize <$> mClientG)
            showBuffer clientGSent     "  sent:     "
            mServerS <- Ref.read serverS
            log $ "serverS: " <> show (ShowableJson <$> mServerS)
            showBuffer serverSReceived "  received: "
            mClientD <- Ref.read clientD
            log $ "clientD: " <> show (ShowableJson <<< serialize <$> mClientD)
            showBuffer clientDSent     "  sent:     "
            mServerG <- Ref.read serverG
            log $ "serverG: " <> show (ShowableJson <<< serialize <$> mServerG)
            showBuffer serverGReceived "  received: "
            mClientS <- Ref.read clientS
            log $ "clientS: " <> show (ShowableJson <$> mClientS)
            showBuffer clientSSent     "  sent:     "
            mServerD <- Ref.read serverD
            log $ "serverD: " <> show (ShowableJson <<< serialize <$> mServerD)
            showBuffer serverDReceived "  received: "
      in  runExists go ex
  where
    showBuffer bufRef prefix = do
      mBuf <- Ref.read bufRef
      log $ prefix <> show mBuf
      let utf8Buf = case mBuf of
            Nothing -> Nothing
            Just (ShowableBuffer x) -> Just (toUtf8String x)
      log $ prefix <> show utf8Buf
