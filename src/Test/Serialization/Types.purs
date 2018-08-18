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
import Control.Monad.Reader (ReaderT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref, newRef)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
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
