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
import Data.Enum (enumFromTo)
import Data.Generic (class Generic, gShow, gEq, gCompare)
import Data.NonEmpty (NonEmpty (..))
import Data.String.Yarn as String
import Data.Exists (Exists, mkExists, runExists)
import Type.Proxy (Proxy (..))
import Control.Alternative ((<|>))
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (evalState)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef, modifyRef)
import Control.Monad.Eff.Random (RANDOM)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, unGen, oneOf, arrayOf1, elements)
import Test.QuickCheck.LCG (randomSeed)
import Unsafe.Coerce (unsafeCoerce)
import Node.Buffer (Buffer)


newtype TestTopic = TestTopic String

derive instance genericTestTopic :: Generic TestTopic

instance eqTestTopic :: Eq TestTopic where
  eq = gEq

instance ordTestTopic :: Ord TestTopic where
  compare = gCompare

instance showTestTopic :: Show TestTopic where
  show = gShow

instance arbitraryTestTopic :: Arbitrary TestTopic where
  arbitrary = TestTopic <$> arbitraryNonEmptyText
    where
      arbitraryNonEmptyText = String.fromChars
                           <$> arrayOf1 (elements $ NonEmpty 'a' $ enumFromTo 'b' 'z')

instance encodeJsonTestTopic :: EncodeJson TestTopic where
  encodeJson (TestTopic x) = encodeJson x

instance decodeJsonTestTopic :: DecodeJson TestTopic where
  decodeJson json = TestTopic <$> decodeJson json


data MsgType
  = GeneratedInput
  | Serialized
  | DeSerialized
  | Failure

derive instance genericMsgType :: Generic MsgType

instance eqMsgType :: Eq MsgType where
  eq = gEq

instance arbitraryMsgType :: Arbitrary MsgType where
  arbitrary = oneOf $ NonEmpty
    (pure GeneratedInput)
    [ pure Serialized
    , pure DeSerialized
    , pure Failure
    ]

instance encodeJsonMsgType :: EncodeJson MsgType where
  encodeJson x = encodeJson $ case x of
    GeneratedInput -> "generated"
    Serialized -> "serialized"
    DeSerialized -> "deserialized"
    Failure -> "failure"

instance decodeJsonMsgType :: DecodeJson MsgType where
  decodeJson json = do
    s <- decodeJson json
    case unit of
      _ | s == "generated" -> pure GeneratedInput
        | s == "serialized" -> pure Serialized
        | s == "deserialized" -> pure DeSerialized
        | s == "failure" -> pure Failure
        | otherwise -> fail "MsgType"


arbitraryJson :: Gen Json
arbitraryJson = oneOf $ NonEmpty
  ((encodeJson :: Int -> Json) <$> arbitrary)
  []


data ClientToServer
  = GetTopics
  | ClientToServer TestTopic MsgType Json
  | ClientToServerBadParse String
  | Finished TestTopic

derive instance genericClientToServer :: Generic ClientToServer

instance eqClientToServer :: Eq ClientToServer where
  eq = gEq

instance arbitraryClientToServer :: Arbitrary ClientToServer where
  arbitrary = oneOf $ NonEmpty
    (pure GetTopics)
    [ ClientToServer <$> arbitrary <*> arbitrary <*> arbitraryJson
    , ClientToServerBadParse <$> arbitraryNonEmptyText
    , Finished <$> arbitrary
    ]
    where
      arbitraryNonEmptyText = String.fromChars
                           <$> arrayOf1 (elements $ NonEmpty 'a' $ enumFromTo 'b' 'z')

instance encodeJsonClientToServer :: EncodeJson ClientToServer where
  encodeJson x = case x of
    GetTopics -> encodeJson "getTopics"
    ClientToServer t m y -> "topic" := t ~> "msgType" := m ~> "value" := y ~> jsonEmptyObject
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
          let msg = ClientToServer <$> o .? "topic" <*> o .? "msgType" <*> o .? "value"
              bad = ClientToServerBadParse <$> o .? "badParse"
              fin = Finished <$> o .? "finished"
          msg <|> bad <|> fin
    str <|> obj


data ServerToClient
  = TopicsAvailable (Set TestTopic)
  | ServerToClient TestTopic MsgType Json
  | ServerToClientBadParse String
  | Continue TestTopic

derive instance genericServerToClient :: Generic ServerToClient

instance eqServerToClient :: Eq ServerToClient where
  eq = gEq

instance arbitraryServerToClient :: Arbitrary ServerToClient where
  arbitrary = oneOf $ NonEmpty
    (TopicsAvailable <<< (Set.fromFoldable :: Array _ -> _) <$> arbitrary)
    [ ServerToClient <$> arbitrary <*> arbitrary <*> arbitraryJson
    , ServerToClientBadParse <$> arbitraryNonEmptyText
    , Continue <$> arbitrary
    ]
    where
      arbitraryNonEmptyText = String.fromChars
                           <$> arrayOf1 (elements $ NonEmpty 'a' $ enumFromTo 'b' 'z')

instance encodeJsonServerToClient :: EncodeJson ServerToClient where
  encodeJson x = case x of
    TopicsAvailable y -> "topics" := (Set.toUnfoldable y :: Array _) ~> jsonEmptyObject
    ServerToClient t m y -> "topic" := t ~> "msgType" := m ~> "value" := y ~> jsonEmptyObject
    ServerToClientBadParse y -> "badParse" := y ~> jsonEmptyObject
    Continue y -> "continue" := y ~> jsonEmptyObject

instance decodeJsonServerToClient :: DecodeJson ServerToClient where
  decodeJson json = do
    o <- decodeJson json
    let msg = ServerToClient <$> o .? "topic" <*> o .? "msgType" <*> o .? "value"
        bad = ServerToClientBadParse <$> o .? "badParse"
        top = TopicsAvailable <<< (Set.fromFoldable :: Array _ -> _) <$> o .? "topics"
        con = Continue <$> o .? "continue"
    msg <|> bad <|> top <|> con



newtype TestTopicState a = TestTopicState
  { generate :: Gen a
  , serialize :: a -> Json
  , deserialize :: Json -> Either String a
  , eq :: a -> a -> Boolean
  , size :: Ref Int
  , serverG :: Ref (Maybe a)
  , serverGReceived :: Ref (Maybe Buffer)
  , clientS :: Ref (Maybe Json)
  , clientSSent :: Ref (Maybe Buffer)
  , serverD :: Ref (Maybe a)
  , serverDReceived :: Ref (Maybe Buffer)
  , clientG :: Ref (Maybe a)
  , clientGSent :: Ref (Maybe Buffer)
  , serverS :: Ref (Maybe Json)
  , serverSReceived :: Ref (Maybe Buffer)
  , clientD :: Ref (Maybe a)
  , clientDSent :: Ref (Maybe Buffer)
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
  serverGReceived <- newRef Nothing
  clientS <- newRef Nothing
  clientSSent <- newRef Nothing
  (serverD :: Ref (Maybe a)) <- newRef Nothing
  serverDReceived <- newRef Nothing
  (clientG :: Ref (Maybe a)) <- newRef Nothing
  clientGSent <- newRef Nothing
  serverS <- newRef Nothing
  serverSReceived <- newRef Nothing
  (clientD :: Ref (Maybe a)) <- newRef Nothing
  clientDSent <- newRef Nothing
  pure $ mkExists $ TestTopicState
    { generate: arbitrary
    , serialize: encodeJson
    , deserialize: decodeJson
    , eq: eq
    , size
    , serverG
    , serverGReceived
    , clientS
    , clientSSent
    , serverD
    , serverDReceived
    , clientG
    , clientGSent
    , serverS
    , serverSReceived
    , clientD
    , clientDSent
    }


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

derive instance genericHasTopic :: Generic a => Generic (HasTopic a)

instance showHasTopic :: Generic a => Show (HasTopic a) where
  show = gShow

instance isOkayHasTopic :: IsOkay a => IsOkay (HasTopic a) where
  isOkay x = case x of
    NoTopic -> false
    HasTopic y -> isOkay y


data GenValue a
  = DoneGenerating
  | GenValue a

derive instance genericGenValue :: Generic a => Generic (GenValue a)

instance showGenValue :: Generic a => Show (GenValue a) where
  show = gShow

instance isOkayGenValue :: IsOkay a => IsOkay (GenValue a) where
  isOkay x = case x of
    DoneGenerating -> false
    GenValue y -> isOkay y


data GotClientGenValue a
  = NoClientGenValue
  | GotClientGenValue a

derive instance genericGotClientGenValue :: Generic a => Generic (GotClientGenValue a)

instance showGotClientGenValue :: Generic a => Show (GotClientGenValue a) where
  show = gShow

instance isOkayGotClientGenValue :: IsOkay a => IsOkay (GotClientGenValue a) where
  isOkay x = case x of
    NoClientGenValue -> false
    GotClientGenValue y -> isOkay y


data HasClientG a
  = NoClientG
  | HasClientG a

derive instance genericHasClientG :: Generic a => Generic (HasClientG a)

instance showHasClientG :: Generic a => Show (HasClientG a) where
  show = gShow

instance isOkayHasClientG :: IsOkay a => IsOkay (HasClientG a) where
  isOkay x = case x of
    NoClientG -> false
    HasClientG y -> isOkay y


data HasServerG a
  = NoServerG
  | HasServerG a

derive instance genericHasServerG :: Generic a => Generic (HasServerG a)

instance showHasServerG :: Generic a => Show (HasServerG a) where
  show = gShow

instance isOkayHasServerG :: IsOkay a => IsOkay (HasServerG a) where
  isOkay x = case x of
    NoServerG -> false
    HasServerG y -> isOkay y


data HasServerS a
  = NoServerS
  | HasServerS a

derive instance genericHasServerS :: Generic a => Generic (HasServerS a)

instance showHasServerS :: Generic a => Show (HasServerS a) where
  show = gShow

instance isOkayHasServerS :: IsOkay a => IsOkay (HasServerS a) where
  isOkay x = case x of
    NoServerS -> false
    HasServerS y -> isOkay y


data HasServerD a
  = NoServerD
  | HasServerD a

derive instance genericHasServerD :: Generic a => Generic (HasServerD a)

instance showHasServerD :: Generic a => Show (HasServerD a) where
  show = gShow

instance isOkayHasServerD :: IsOkay a => IsOkay (HasServerD a) where
  isOkay x = case x of
    NoServerD -> false
    HasServerD y -> isOkay y


data HasClientD a
  = NoClientD
  | HasClientD a

derive instance genericHasClientD :: Generic a => Generic (HasClientD a)

instance showHasClientD :: Generic a => Show (HasClientD a) where
  show = gShow

instance isOkayHasClientD :: IsOkay a => IsOkay (HasClientD a) where
  isOkay x = case x of
    NoClientD -> false
    HasClientD y -> isOkay y


data DesValue a
  = CantDes String
  | DesValue a

derive instance genericDesValue :: Generic a => Generic (DesValue a)

instance showDesValue :: Generic a => Show (DesValue a) where
  show = gShow

instance isOkayDesValue :: IsOkay a => IsOkay (DesValue a) where
  isOkay x = case x of
    CantDes _ -> false
    DesValue y -> isOkay y


data HasClientS a
  = NoClientS
  | HasClientS a

derive instance genericHasClientS :: Generic a => Generic (HasClientS a)

instance showHasClientS :: Generic a => Show (HasClientS a) where
  show = gShow

instance isOkayHasClientS :: IsOkay a => IsOkay (HasClientS a) where
  isOkay x = case x of
    NoClientS -> false
    HasClientS y -> isOkay y


data ServerSerializedMatch a
  = ServerSerializedMatch a
  | ServerSerializedMismatch Json Json

derive instance genericServerSerializedMatch :: Generic a => Generic (ServerSerializedMatch a)

instance showServerSerializedMatch :: Generic a => Show (ServerSerializedMatch a) where
  show = gShow

instance isOkayServerSerializedMatch :: IsOkay a => IsOkay (ServerSerializedMatch a) where
  isOkay x = case x of
    ServerSerializedMismatch _ _ -> false
    ServerSerializedMatch y -> isOkay y


data ServerDeSerializedMatch a
  = ServerDeSerializedMatch a
  | ServerDeSerializedMismatch Json Json

derive instance genericServerDeSerializedMatch :: Generic a => Generic (ServerDeSerializedMatch a)

instance showServerDeSerializedMatch :: Generic a => Show (ServerDeSerializedMatch a) where
  show = gShow

instance isOkayServerDeSerializedMatch :: IsOkay a => IsOkay (ServerDeSerializedMatch a) where
  isOkay x = case x of
    ServerDeSerializedMismatch _ _ -> false
    ServerDeSerializedMatch y -> isOkay y


data ClientSerializedMatch a
  = ClientSerializedMatch a
  | ClientSerializedMismatch Json Json

derive instance genericClientSerializedMatch :: Generic a => Generic (ClientSerializedMatch a)

instance showClientSerializedMatch :: Generic a => Show (ClientSerializedMatch a) where
  show = gShow

instance isOkayClientSerializedMatch :: IsOkay a => IsOkay (ClientSerializedMatch a) where
  isOkay x = case x of
    ClientSerializedMismatch _ _ -> false
    ClientSerializedMatch y -> isOkay y


data ClientDeSerializedMatch a
  = ClientDeSerializedMatch a
  | ClientDeSerializedMismatch Json Json

derive instance genericClientDeSerializedMatch :: Generic a => Generic (ClientDeSerializedMatch a)

instance showClientDeSerializedMatch :: Generic a => Show (ClientDeSerializedMatch a) where
  show = gShow

instance isOkayClientDeSerializedMatch :: IsOkay a => IsOkay (ClientDeSerializedMatch a) where
  isOkay x = case x of
    ClientDeSerializedMismatch _ _ -> false
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
               . Exists TestTopicState
              -> TestTopic
              -> Eff
                 ( ref :: REF
                 , random :: RANDOM
                 | eff) (GenValue ClientToServer)
generateValue ex topic =
  let go :: forall a -- . Arbitrary a => EncodeJson a => DecodeJson a => Eq a
          . TestTopicState a -> Eff (ref :: REF, random :: RANDOM | eff) (GenValue ClientToServer)
      go (TestTopicState {size,generate,serialize,clientG}) = do
        s <- readRef size
        if s >= 100
          then pure DoneGenerating
          else do
            g <- randomSeed
            let val = evalState (unGen generate) {newSeed: g, size: s}
            modifyRef size (\x -> x + 1)
            writeRef clientG (Just val)
            pure $ GenValue $ ClientToServer topic GeneratedInput $ serialize val
  in  runExists go ex
 

gotServerGenValue :: forall eff
                   . Exists TestTopicState
                  -> Json
                  -> Eff (ref :: REF | eff) (DesValue Unit)
gotServerGenValue ex value =
  let go :: forall a -- . Arbitrary a => EncodeJson a => DecodeJson a => Eq a
          . TestTopicState a -> Eff (ref :: REF | eff) (DesValue Unit)
      go (TestTopicState {deserialize,serverG}) = case deserialize value of
        Left e -> pure (CantDes e)
        Right y -> do
          writeRef serverG (Just y)
          pure (DesValue unit)
  in  runExists go ex


serializeValueServerOrigin :: forall eff
                            . Exists TestTopicState
                           -> TestTopic
                           -> Eff (ref :: REF | eff) (HasServerG ClientToServer)
serializeValueServerOrigin ex topic =
  let go :: forall a -- . Arbitrary a => EncodeJson a => DecodeJson a => Eq a
          . TestTopicState a -> Eff (ref :: REF | eff) (HasServerG ClientToServer)
      go (TestTopicState {serialize,serverG,clientS}) = do
        mX <- readRef serverG
        case mX of
          Nothing -> pure NoServerG
          Just x -> map HasServerG $ do
            let val = serialize x
            writeRef clientS (Just val)
            pure $ ClientToServer topic Serialized val
  in  runExists go ex


gotServerSerialize :: forall eff
                    . Exists TestTopicState
                   -> Json
                   -> Eff (ref :: REF | eff) Unit
gotServerSerialize ex value =
  let go :: forall a -- . Arbitrary a => EncodeJson a => DecodeJson a => Eq a
          . TestTopicState a -> Eff (ref :: REF | eff) Unit
      go (TestTopicState {deserialize,serverS}) = do
        writeRef serverS (Just value)
  in  runExists go ex


deserializeValueServerOrigin :: forall eff
                              . Exists TestTopicState
                             -> TestTopic
                             -> Eff (ref :: REF | eff) (HasServerS (DesValue ClientToServer))
deserializeValueServerOrigin ex topic =
  let go :: forall a -- . Arbitrary a => EncodeJson a => DecodeJson a => Eq a
          . TestTopicState a -> Eff (ref :: REF | eff) (HasServerS (DesValue ClientToServer))
      go (TestTopicState {deserialize,serverS,clientD,serialize}) = do
        mX <- readRef serverS
        case mX of
          Nothing -> pure NoServerS
          Just x -> map HasServerS $ case deserialize x of
            Left e -> pure (CantDes e)
            Right y -> do
              writeRef clientD (Just y)
              pure $ DesValue $ ClientToServer topic DeSerialized $ serialize y
  in  runExists go ex


gotServerDeSerialize :: forall eff
                      . Exists TestTopicState
                     -> Json
                     -> Eff (ref :: REF | eff) (DesValue Unit)
gotServerDeSerialize ex value =
  let go :: forall a -- . Arbitrary a => EncodeJson a => DecodeJson a => Eq a
          . TestTopicState a -> Eff (ref :: REF | eff) (DesValue Unit)
      go (TestTopicState {deserialize,serverD}) = case deserialize value of
        Left e -> pure (CantDes e)
        Right y -> do
          writeRef serverD (Just y)
          pure (DesValue unit)
  in  runExists go ex


verify :: forall eff
        . Exists TestTopicState
       -> Eff (ref :: REF | eff)
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
                                ( ServerDeSerializedMatch Unit))))))))))))
verify ex =
  let go :: forall a -- . Arbitrary a => EncodeJson a => DecodeJson a => Eq a
          . TestTopicState a -> Eff (ref :: REF | eff) _
      go (TestTopicState {serialize,deserialize,eq,clientG,serverS,clientD,serverG,clientS,serverD}) = do
        mClientG <- readRef clientG
        case mClientG of
          Nothing -> pure NoClientG
          Just clientG' -> map HasClientG $ do
            mServerS <- readRef serverS
            case mServerS of
              Nothing -> pure NoServerS
              Just serverS' -> map HasServerS $ do
                if serialize clientG' /= serverS'
                  then pure $ ClientSerializedMismatch (serialize clientG') serverS'
                  else map ClientSerializedMatch $ do
                    mClientD <- readRef clientD
                    case mClientD of
                      Nothing -> pure NoClientD
                      Just clientD' -> map HasClientD $ do
                        case deserialize serverS' of
                          Left e -> pure (CantDes e)
                          Right clientD''
                            | not (eq clientD'' clientD') -> pure $ DesValue $ ClientDeSerializedMismatch (serialize clientD'') (serialize clientD')
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
                                          then pure $ ServerSerializedMismatch (serialize serverG') clientS'
                                          else map ServerSerializedMatch $ do
                                            mServerD <- readRef serverD
                                            case mServerD of
                                              Nothing -> pure NoServerD
                                              Just serverD' -> map HasServerD $ do
                                                case deserialize clientS' of
                                                  Left e -> pure (CantDes e)
                                                  Right serverS''
                                                    | not (eq serverS'' serverD') -> pure $ DesValue $ ServerDeSerializedMismatch (serialize serverS'') (serialize serverD')
                                                    | otherwise -> pure $ DesValue $ ServerDeSerializedMatch $ unit
  in  runExists go ex
