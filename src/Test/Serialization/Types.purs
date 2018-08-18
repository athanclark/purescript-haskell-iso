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

instance encodeJsonClientToServer :: EncodeJson ClientToServer where
  encodeJson x = case x of
    GetTopics -> encodeJson "getTopics"
    ClientToServer y -> "channelMsg" := y ~> jsonEmptyObject
    ClientToServerBadParse y -> "badParse" := y ~> jsonEmptyObject

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
          msg <|> bad
    str <|> obj


data ServerToClient
  = TopicsAvailable (Set TestTopic)
  | ServerToClient ChannelMsg
  | ServerToClientBadParse String

instance encodeJsonServerToClient :: EncodeJson ServerToClient where
  encodeJson x = case x of
    TopicsAvailable y -> "topics" := (Set.toUnfoldable y :: Array TestTopic) ~> jsonEmptyObject
    ServerToClient y -> "channelMsg" := y ~> jsonEmptyObject
    ServerToClientBadParse y -> "badParse" := y ~> jsonEmptyObject

instance decodeJsonServerToClient :: DecodeJson ServerToClient where
  decodeJson json = do
    o <- decodeJson json
    let msg = ServerToClient <$> o .? "channelMsg"
        bad = ServerToClientBadParse <$> o .? "badParse"
        top = (TopicsAvailable <<< Set.fromFoldable :: Array TestTopic -> Set TestTopic) <$> o .? "topics"
    msg <|> bad <|> top



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


data HasTopic a
  = HasTopic a
  | NoTopic


data GenValue a
  = DoneGenerating
  | GenValue a


data GotClientGenValue a
  = NoClientGenValue
  | GotClientGenValue a


data HasClientG a
  = NoClientG
  | HasClientG a


data HasServerG a
  = NoServerG
  | HasServerG a


data HasServerS a
  = NoServerS
  | HasServerS a


data HasServerD a
  = NoServerD
  | HasServerD a


data HasClientD a
  = NoClientD
  | HasClientD a


data DesValue a
  = CantDes String
  | DesValue a


data HasClientS a
  = NoClientS
  | HasClientS a


data ServerSerializedMatch a
  = ServerSerializedMatch a
  | ServerSerializedMismatch


data ServerDeSerializedMatch a
  = ServerDeSerializedMatch a
  | ServerDeSerializedMismatch


data ClientSerializedMatch a
  = ClientSerializedMatch a
  | ClientSerializedMismatch


data ClientDeSerializedMatch a
  = ClientDeSerializedMatch a
  | ClientDeSerializedMismatch



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
          go (TestTopicState {size,generate,serialize,serverG}) = do
            s <- readRef size
            if s >= 100
              then pure DoneGenerating
              else do
                g <- randomSeed
                let val = evalState (unGen generate) {newSeed: g, size: s}
                modifyRef size (\x -> x + 1)
                writeRef serverG (Just val)
                pure $ GenValue $ GeneratedInput topic $ serialize val
      in  HasTopic <$> runExists go ex
