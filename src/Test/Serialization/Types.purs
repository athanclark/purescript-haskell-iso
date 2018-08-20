module Test.Serialization.Types where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Argonaut
  ( Json, class EncodeJson, class DecodeJson, decodeJson, encodeJson
  , (.?), (:=), (~>), jsonEmptyObject, fail)
import Data.Set (Set)
import Data.Set as Set
import Data.Map (Map)
import Data.Map as Map
import Type.Proxy (Proxy (..))
import Control.Alternative ((<|>))
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (evalState)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef, modifyRef)
import Control.Monad.Eff.Random (RANDOM)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, unGen)
import Test.QuickCheck.LCG (randomSeed)
import Unsafe.Coerce (unsafeCoerce)


type TestTopic = String


data ChannelMsg
  = GeneratedInput TestTopic Json
  | Serialized TestTopic Json
  | DeSerialized TestTopic Json
  | Failure TestTopic Json

instance encodeJsonChannelMsg :: EncodeJson ChannelMsg where
  encodeJson x = case x of
    GeneratedInput t y -> "topic" := t ~> "generated" := y ~> jsonEmptyObject
    Serialized t y -> "topic" := t ~> "serialized" := y ~> jsonEmptyObject
    DeSerialized t y -> "topic" := t ~> "deserialized" := y ~> jsonEmptyObject
    Failure t y -> "topic" := t ~> "failure" := y ~> jsonEmptyObject

instance decodeJsonChannelMsg :: DecodeJson ChannelMsg where
  decodeJson json = do
    o <- decodeJson json
    let gen = GeneratedInput <$> o .? "topic" <*> o .? "generated"
        ser = Serialized <$> o .? "topic" <*> o .? "serialized"
        des = DeSerialized <$> o .? "topic" <*> o .? "deserialized"
        fai = Failure <$> o .? "topic" <*> o .? "failure"
    gen <|> ser <|> des <|> fai


data ClientToServer
  = GetTopics
  | ClientToServer ChannelMsg
  | ClientToServerBadParse String
  | Finished TestTopic

instance encodeJsonClientToServer :: EncodeJson ClientToServer where
  encodeJson x = case x of
    GetTopics -> encodeJson "getTopics"
    ClientToServer y -> "channelMsg" := y ~> jsonEmptyObject
    ClientToServerBadParse y -> "badParse" := y ~> jsonEmptyObject
    Finished y -> "finished" := y ~> jsonEmptyObject

instance decodeJsonClientToServer :: DecodeJson ClientToServer where
  decodeJson json = do
    let str = do
          s <- decodeJson json
          case unit of
            _ | s == "getTopics" -> pure GetTopics
              | otherwise -> fail "ClientToServer"
        obj = do
          o <- decodeJson json
          let msg = ClientToServer <$> o .? "channelMsg"
              bad = ClientToServerBadParse <$> o .? "badParse"
              fin = Finished <$> o .? "finished"
          msg <|> bad <|> fin
    str <|> obj


data ServerToClient
  = TopicsAvailable (Set TestTopic)
  | ServerToClient ChannelMsg
  | ServerToClientBadParse String
  | Continue TestTopic

instance encodeJsonServerToClient :: EncodeJson ServerToClient where
  encodeJson x = case x of
    TopicsAvailable y -> "topics" := (Set.toUnfoldable y :: Array TestTopic) ~> jsonEmptyObject
    ServerToClient y -> "channelMsg" := y ~> jsonEmptyObject
    ServerToClientBadParse y -> "badParse" := y ~> jsonEmptyObject
    Continue y -> "continue" := y ~> jsonEmptyObject

instance decodeJsonServerToClient :: DecodeJson ServerToClient where
  decodeJson json = do
    o <- decodeJson json
    let msg = ServerToClient <$> o .? "channelMsg"
        bad = ServerToClientBadParse <$> o .? "badParse"
        top = (TopicsAvailable <<< Set.fromFoldable :: Array TestTopic -> Set TestTopic) <$> o .? "topics"
        con = Continue <$> o .? "continue"
    msg <|> bad <|> top <|> con



newtype TestTopicState a = TestTopicState
  { generate :: Gen a
  , serialize :: a -> Json
  , deserialize :: Json -> Either String a
  , size :: Ref Int
  , serverG :: Ref (Maybe a)
  , clientS :: Ref (Maybe Json)
  , serverD :: Ref (Maybe a)
  , clientG :: Ref (Maybe a)
  , serverS :: Ref (Maybe Json)
  , clientD :: Ref (Maybe a)
  }


emptyTestTopicState :: forall a eff
                     . Arbitrary a
                    => EncodeJson a
                    => DecodeJson a
                    => Eq a
                    => Proxy a -> Eff (ref :: REF | eff) (Exists TestTopicState)
emptyTestTopicState Proxy = do
  size <- newRef 1
  (serverG :: Ref (Maybe a)) <- newRef Nothing
  clientS <- newRef Nothing
  (serverD :: Ref (Maybe a)) <- newRef Nothing
  (clientG :: Ref (Maybe a)) <- newRef Nothing
  serverS <- newRef Nothing
  (clientD :: Ref (Maybe a)) <- newRef Nothing
  pure $ mkExists $ TestTopicState
    { generate: arbitrary
    , serialize: encodeJson
    , deserialize: decodeJson
    , size
    , serverG
    , clientS
    , serverD
    , clientG
    , serverS
    , clientD
    }


foreign import data Exists :: (Type -> Type) -> Type

mkExists :: forall f a. Arbitrary a => EncodeJson a => DecodeJson a => Eq a => f a -> Exists f
mkExists = unsafeCoerce

runExists :: forall f r. (forall a. Arbitrary a => EncodeJson a => DecodeJson a => Eq a => f a -> r) -> Exists f -> r
runExists = unsafeCoerce


type TestSuiteState = Ref (Map TestTopic (Exists TestTopicState))

emptyTestSuiteState :: forall eff. Eff (ref :: REF | eff) TestSuiteState
emptyTestSuiteState = newRef Map.empty


type TestSuiteM eff a = ReaderT TestSuiteState (Eff eff) a


registerTopic :: forall a eff
               . Arbitrary a => EncodeJson a => DecodeJson a => Eq a
              => TestTopic -> Proxy a -> TestSuiteM (ref :: REF | eff) Unit
registerTopic topic p = do
  xsRef <- ask
  liftEff $ do
    state <- emptyTestTopicState p
    modifyRef xsRef (Map.insert topic state)


class IsOkay a where
  isOkay :: a -> Boolean

instance isOkayUnit :: IsOkay Unit where
  isOkay _ = true


data HasTopic a
  = HasTopic a
  | NoTopic

instance isOkayHasTopic :: IsOkay a => IsOkay (HasTopic a) where
  isOkay x = case x of
    NoTopic -> false
    HasTopic y -> isOkay y

data GenValue a
  = DoneGenerating
  | GenValue a

instance isOkayGenValue :: IsOkay a => IsOkay (GenValue a) where
  isOkay x = case x of
    DoneGenerating -> false
    GenValue y -> isOkay y


data GotClientGenValue a
  = NoClientGenValue
  | GotClientGenValue a

instance isOkayGotClientGenValue :: IsOkay a => IsOkay (GotClientGenValue a) where
  isOkay x = case x of
    NoClientGenValue -> false
    GotClientGenValue y -> isOkay y


data HasClientG a
  = NoClientG
  | HasClientG a

instance isOkayHasClientG :: IsOkay a => IsOkay (HasClientG a) where
  isOkay x = case x of
    NoClientG -> false
    HasClientG y -> isOkay y


data HasServerG a
  = NoServerG
  | HasServerG a

instance isOkayHasServerG :: IsOkay a => IsOkay (HasServerG a) where
  isOkay x = case x of
    NoServerG -> false
    HasServerG y -> isOkay y


data HasServerS a
  = NoServerS
  | HasServerS a

instance isOkayHasServerS :: IsOkay a => IsOkay (HasServerS a) where
  isOkay x = case x of
    NoServerS -> false
    HasServerS y -> isOkay y


data HasServerD a
  = NoServerD
  | HasServerD a

instance isOkayHasServerD :: IsOkay a => IsOkay (HasServerD a) where
  isOkay x = case x of
    NoServerD -> false
    HasServerD y -> isOkay y


data HasClientD a
  = NoClientD
  | HasClientD a

instance isOkayHasClientD :: IsOkay a => IsOkay (HasClientD a) where
  isOkay x = case x of
    NoClientD -> false
    HasClientD y -> isOkay y


data DesValue a
  = CantDes String
  | DesValue a

instance isOkayDesValue :: IsOkay a => IsOkay (DesValue a) where
  isOkay x = case x of
    CantDes _ -> false
    DesValue y -> isOkay y


data HasClientS a
  = NoClientS
  | HasClientS a

instance isOkayHasClientS :: IsOkay a => IsOkay (HasClientS a) where
  isOkay x = case x of
    NoClientS -> false
    HasClientS y -> isOkay y


data ServerSerializedMatch a
  = ServerSerializedMatch a
  | ServerSerializedMismatch

instance isOkayServerSerializedMatch :: IsOkay a => IsOkay (ServerSerializedMatch a) where
  isOkay x = case x of
    ServerSerializedMismatch -> false
    ServerSerializedMatch y -> isOkay y


data ServerDeSerializedMatch a
  = ServerDeSerializedMatch a
  | ServerDeSerializedMismatch

instance isOkayServerDeSerializedMatch :: IsOkay a => IsOkay (ServerDeSerializedMatch a) where
  isOkay x = case x of
    ServerDeSerializedMismatch -> false
    ServerDeSerializedMatch y -> isOkay y


data ClientSerializedMatch a
  = ClientSerializedMatch a
  | ClientSerializedMismatch

instance isOkayClientSerializedMatch :: IsOkay a => IsOkay (ClientSerializedMatch a) where
  isOkay x = case x of
    ClientSerializedMismatch -> false
    ClientSerializedMatch y -> isOkay y


data ClientDeSerializedMatch a
  = ClientDeSerializedMatch a
  | ClientDeSerializedMismatch

instance isOkayClientDeSerializedMatch :: IsOkay a => IsOkay (ClientDeSerializedMatch a) where
  isOkay x = case x of
    ClientDeSerializedMismatch -> false
    ClientDeSerializedMatch y -> isOkay y



getTopicState :: forall eff
               . TestSuiteState
              -> TestTopic
              -> Eff (ref :: REF | eff) (HasTopic (Exists TestTopicState))
getTopicState xsRef topic = do
  xs <- readRef xsRef
  case Map.lookup topic xs of
    Nothing -> pure NoTopic
    Just x -> pure (HasTopic x)


generateValue :: forall eff
               . TestSuiteState
              -> TestTopic
              -> Eff
                 ( ref :: REF
                 , random :: RANDOM
                 | eff) (HasTopic (GenValue ChannelMsg))
generateValue xsRef topic = do
  mState <- getTopicState xsRef topic
  case mState of
    NoTopic -> pure NoTopic
    HasTopic ex ->
      let go :: forall a. Arbitrary a => EncodeJson a => DecodeJson a => Eq a
             => TestTopicState a -> Eff (ref :: REF, random :: RANDOM | eff) (GenValue ChannelMsg)
          go (TestTopicState {size,generate,serialize,clientG}) = do
            s <- readRef size
            if s >= 100
              then pure DoneGenerating
              else do
                g <- randomSeed
                let val = evalState (unGen generate) {newSeed: g, size: s}
                modifyRef size (\x -> x + 1)
                writeRef clientG (Just val)
                pure $ GenValue $ GeneratedInput topic $ serialize val
      in  HasTopic <$> runExists go ex
 

gotServerGenValue :: forall eff
                   . TestSuiteState
                  -> TestTopic
                  -> Json
                  -> Eff (ref :: REF | eff) (HasTopic (DesValue Unit))
gotServerGenValue xsRef topic value = do
  mState <- getTopicState xsRef topic
  case mState of
    NoTopic -> pure NoTopic
    HasTopic ex ->
      let go :: forall a. Arbitrary a => EncodeJson a => DecodeJson a => Eq a
             => TestTopicState a -> Eff (ref :: REF | eff) (DesValue Unit)
          go (TestTopicState {deserialize,serverG}) = case deserialize value of
            Left e -> pure (CantDes e)
            Right y -> do
              writeRef serverG (Just y)
              pure (DesValue unit)
      in  HasTopic <$> runExists go ex


serializeValueServerOrigin :: forall eff
                            . TestSuiteState
                           -> TestTopic
                           -> Eff (ref :: REF | eff) (HasTopic (HasServerG ChannelMsg))
serializeValueServerOrigin xsRef topic = do
  mState <- getTopicState xsRef topic
  case mState of
    NoTopic -> pure NoTopic
    HasTopic ex ->
      let go :: forall a. Arbitrary a => EncodeJson a => DecodeJson a => Eq a
             => TestTopicState a -> Eff (ref :: REF | eff) (HasServerG ChannelMsg)
          go (TestTopicState {serialize,serverG,clientS}) = do
            mX <- readRef serverG
            case mX of
              Nothing -> pure NoServerG
              Just x -> map HasServerG $ do
                let val = serialize x
                writeRef clientS (Just val)
                pure $ Serialized topic val
      in  HasTopic <$> runExists go ex


gotServerSerialize :: forall eff
                    . TestSuiteState
                   -> TestTopic
                   -> Json
                   -> Eff (ref :: REF | eff) (HasTopic Unit)
gotServerSerialize xsRef topic value = do
  mState <- getTopicState xsRef topic
  case mState of
    NoTopic -> pure NoTopic
    HasTopic ex ->
      let go :: forall a. Arbitrary a => EncodeJson a => DecodeJson a => Eq a
             => TestTopicState a -> Eff (ref :: REF | eff) Unit
          go (TestTopicState {deserialize,serverS}) = do
            writeRef serverS (Just value)
      in  HasTopic <$> runExists go ex


deserializeValueServerOrigin :: forall eff
                              . TestSuiteState
                             -> TestTopic
                             -> Eff (ref :: REF | eff) (HasTopic (HasServerS (DesValue ChannelMsg)))
deserializeValueServerOrigin xsRef topic = do
  mState <- getTopicState xsRef topic
  case mState of
    NoTopic -> pure NoTopic
    HasTopic ex ->
      let go :: forall a. Arbitrary a => EncodeJson a => DecodeJson a => Eq a
             => TestTopicState a -> Eff (ref :: REF | eff) (HasServerS (DesValue ChannelMsg))
          go (TestTopicState {deserialize,serverS,serverD,serialize}) = do
            mX <- readRef serverS
            case mX of
              Nothing -> pure NoServerS
              Just x -> map HasServerS $ case deserialize x of
                Left e -> pure (CantDes e)
                Right y -> do
                  writeRef serverD (Just y)
                  pure $ DesValue $ DeSerialized topic $ serialize y
      in  HasTopic <$> runExists go ex


gotClientDeSerialize :: forall eff
                      . TestSuiteState
                     -> TestTopic
                     -> Json
                     -> Eff (ref :: REF | eff) (HasTopic (DesValue Unit))
gotClientDeSerialize xsRef topic value = do
  mState <- getTopicState xsRef topic
  case mState of
    NoTopic -> pure NoTopic
    HasTopic ex ->
      let go :: forall a. Arbitrary a => EncodeJson a => DecodeJson a => Eq a
             => TestTopicState a -> Eff (ref :: REF | eff) (DesValue Unit)
          go (TestTopicState {deserialize,clientD}) = case deserialize value of
            Left e -> pure (CantDes e)
            Right y -> do
              writeRef clientD (Just y)
              pure (DesValue unit)
      in  HasTopic <$> runExists go ex


verify :: forall eff
        . TestSuiteState
       -> TestTopic
       -> Eff (ref :: REF | eff)
          ( HasTopic
            ( HasClientG
              ( HasServerS
                ( ClientSerializedMatch
                  ( HasClientD
                    ( DesValue
                      ( ClientDeSerializedMatch
                        ( HasServerG
                          ( HasClientS
                            ( ServerSerializedMatch
                              ( HasServerD
                                ( DesValue
                                  ( ServerDeSerializedMatch Unit)))))))))))))
verify xsRef topic = do
  mState <- getTopicState xsRef topic
  case mState of
    NoTopic -> pure NoTopic
    HasTopic ex ->
      let go :: forall a. Arbitrary a => EncodeJson a => DecodeJson a => Eq a
             => TestTopicState a -> Eff (ref :: REF | eff) _
          go (TestTopicState {serialize,deserialize,clientG,serverS,clientD,serverG,clientS,serverD}) = do
            mClientG <- readRef clientG
            case mClientG of
              Nothing -> pure NoClientG
              Just clientG' -> map HasClientG $ do
                mServerS <- readRef serverS
                case mServerS of
                  Nothing -> pure NoServerS
                  Just serverS' -> map HasServerS $ do
                    if serialize clientG' /= serverS'
                      then pure ClientSerializedMismatch
                      else map ClientSerializedMatch $ do
                        mClientD <- readRef clientD
                        case mClientD of
                          Nothing -> pure NoClientD
                          Just clientD' -> map HasClientD $ do
                            case deserialize serverS' of
                              Left e -> pure (CantDes e)
                              Right clientD''
                                | clientD'' /= clientD' -> pure (DesValue ClientDeSerializedMismatch)
                                | otherwise -> map (DesValue <<< ClientDeSerializedMatch) $ do
                                    mServerG <- readRef serverG
                                    case mServerG of
                                      Nothing -> pure NoServerG
                                      Just serverG' -> map HasServerG $ do
                                        mClientS <- readRef clientS
                                        case mClientS of
                                          Nothing -> pure NoClientS
                                          Just clientS' -> map HasClientS $ do
                                            if serialize serverG' /= clientS'
                                              then pure ServerSerializedMismatch
                                              else map ServerSerializedMatch $ do
                                                mServerD <- readRef serverD
                                                case mServerD of
                                                  Nothing -> pure NoServerD
                                                  Just serverD' -> map HasServerD $ do
                                                    case deserialize clientS' of
                                                      Left e -> pure (CantDes e)
                                                      Right serverS''
                                                        | serverS'' /= serverD' -> pure (DesValue ServerDeSerializedMismatch)
                                                        | otherwise -> pure $ DesValue $ ServerDeSerializedMatch $ unit
      in  HasTopic <$> runExists go ex
