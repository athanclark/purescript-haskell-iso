module Test.Serialization.Types where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Tuple (Tuple (..))
import Data.Argonaut
  ( Json, class EncodeJson, class DecodeJson, decodeJson, encodeJson
  , (.?), (:=), (~>), jsonEmptyObject, fail)
import Data.Argonaut as Argonaut
import Data.Argonaut.JSONUnit (JSONUnit (..))
import Data.Set (Set)
import Data.Set as Set
import Data.Map (Map)
import Data.Map as Map
import Data.Enum (enumFromTo)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Eq (genericEq)
import Data.NonEmpty (NonEmpty (..))
import Data.String.Yarn as String
import Data.Exists (Exists, mkExists, runExists)
import Data.Array as Array
import Data.Maybe.First (First (..))
import Foreign.Object as Object
import Type.Proxy (Proxy (..))
import Control.Alternative ((<|>))
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (evalState)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, unGen, oneOf, arrayOf1, elements)
import Random.LCG (randomSeed)
import Node.Buffer (Buffer)
import Node.Buffer (fromString, toArray) as Buffer
import Node.Encoding (Encoding (UTF8)) as Buffer



-- * Network Messages

-- | Internally used for testing
newtype TestTopic = TestTopic String
derive instance genericTestTopic :: Generic TestTopic _
derive newtype instance eqTestTopic :: Eq TestTopic
derive newtype instance ordTestTopic :: Ord TestTopic
derive newtype instance showTestTopic :: Show TestTopic
instance arbitraryTestTopic :: Arbitrary TestTopic where
  arbitrary = TestTopic <$> arbitraryNonEmptyText
    where
      arbitraryNonEmptyText = String.fromChars
                           <$> arrayOf1 (elements $ NonEmpty 'a' $ enumFromTo 'b' 'z')
instance encodeJsonTestTopic :: EncodeJson TestTopic where
  encodeJson (TestTopic x) = encodeJson x
instance decodeJsonTestTopic :: DecodeJson TestTopic where
  decodeJson json = TestTopic <$> decodeJson json

-- | The state of affairs on either the sender or recipient of a message
data MsgType
  = GeneratedInput
  | Serialized
  | DeSerialized
  | Failure
derive instance genericMsgType :: Generic MsgType _
instance eqMsgType :: Eq MsgType where
  eq = genericEq
instance showMsgType :: Show MsgType where
  show = genericShow
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


-- FIXME add more options for generating json
arbitraryJson :: Gen Json
arbitraryJson = oneOf $ NonEmpty
  ((encodeJson :: Int -> Json) <$> arbitrary)
  []


-- | All messages from the client to the server. Protocol proceeds as follows:
-- |
-- | 1. Client connects to server
-- | 2. Client asks server for topics it supports
-- | 3. Server responds with set of supported topics
-- | 4. a) If client topics == server topics, generate a test case
-- | 4. b) Client "sends" the test case to server
-- | 4. c) Server "serializes" the interpreted value, sends it to client
-- | 4. d) Client "deserializes" the value, checks if it's equal to the sent value
-- | 4. e) if it's not equal, fail. If it is, continue `n` times.
-- | 5. a) If a bad parse occurs, throw an error and relay the message to the peer
-- | 5. b) Else, Continue until all cases have been generated.
data ClientToServer
  = GetTopics -- ^ Get supported topics
  | ClientToServer TestTopic MsgType ShowableJson -- ^ Genuine test message for a topic
  | ClientToServerBadParse String -- ^ Relaying the inability to parse a message
  | Finished TestTopic -- ^ Test finished successfully for a topic
derive instance genericClientToServer :: Generic ClientToServer _
instance eqClientToServer :: Eq ClientToServer where
  eq = genericEq
instance showClientToServer :: Show ClientToServer where
  show = genericShow
instance arbitraryClientToServer :: Arbitrary ClientToServer where
  arbitrary = oneOf $ NonEmpty
    (pure GetTopics)
    [ ClientToServer <$> arbitrary <*> arbitrary <*> (ShowableJson <$> arbitraryJson)
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


-- | All messages from the server to the client
data ServerToClient
  = TopicsAvailable (Set TestTopic) -- ^ Tells the client which topics it supports
  | ServerToClient TestTopic MsgType ShowableJson -- ^ Genuine test message for a topic
  | ServerToClientBadParse String -- ^ Relaying the inability to parse a message
  | Continue TestTopic -- ^ Give the server another test case, from the client
derive instance genericServerToClient :: Generic ServerToClient _
instance eqServerToClient :: Eq ServerToClient where
  eq = genericEq
instance showServerToClient :: Show ServerToClient where
  show = genericShow
instance arbitraryServerToClient :: Arbitrary ServerToClient where
  arbitrary = oneOf $ NonEmpty
    (TopicsAvailable <<< (Set.fromFoldable :: Array TestTopic -> Set TestTopic) <$> arbitrary)
    [ ServerToClient <$> arbitrary <*> arbitrary <*> (ShowableJson <$> arbitraryJson)
    , ServerToClientBadParse <$> arbitraryNonEmptyText
    , Continue <$> arbitrary
    ]
    where
      arbitraryNonEmptyText = String.fromChars
                           <$> arrayOf1 (elements $ NonEmpty 'a' $ enumFromTo 'b' 'z')
instance encodeJsonServerToClient :: EncodeJson ServerToClient where
  encodeJson x = case x of
    TopicsAvailable y -> "topics" := (Set.toUnfoldable y :: Array TestTopic) ~> jsonEmptyObject
    ServerToClient t m y -> "topic" := t ~> "msgType" := m ~> "value" := y ~> jsonEmptyObject
    ServerToClientBadParse y -> "badParse" := y ~> jsonEmptyObject
    Continue y -> "continue" := y ~> jsonEmptyObject
instance decodeJsonServerToClient :: DecodeJson ServerToClient where
  decodeJson json = do
    o <- decodeJson json
    let msg = ServerToClient <$> o .? "topic" <*> o .? "msgType" <*> o .? "value"
        bad = ServerToClientBadParse <$> o .? "badParse"
        top = TopicsAvailable <<< (Set.fromFoldable :: Array TestTopic -> Set TestTopic) <$> o .? "topics"
        con = Continue <$> o .? "continue"
    msg <|> bad <|> top <|> con




-- * Internal State

-- | Typeclass instances captured in a data type for any `a`.
-- |
-- | `G`: Generated
-- | `S`: Serialized
-- | `D`: Deserialized
newtype TestTopicState a = TestTopicState
  { generate        :: Gen a
  , serialize       :: a -> Json
  , deserialize     :: Json -> Either String a
  , eq              :: a -> a -> Boolean
  , size            :: Ref Int
  , serverG         :: Ref (Maybe a) -- server first generates
  , serverGReceived :: Ref (Maybe ShowableBuffer)
  , clientS         :: Ref (Maybe Json)
  , clientSSent     :: Ref (Maybe ShowableBuffer)
  , serverD         :: Ref (Maybe a)
  , serverDReceived :: Ref (Maybe ShowableBuffer)
  , clientG         :: Ref (Maybe a) -- then client generates
  , clientGSent     :: Ref (Maybe ShowableBuffer)
  , serverS         :: Ref (Maybe Json)
  , serverSReceived :: Ref (Maybe ShowableBuffer)
  , clientD         :: Ref (Maybe a)
  , clientDSent     :: Ref (Maybe ShowableBuffer)
  }


-- | Initial state for a topic
emptyTestTopicState :: forall a
                     . Arbitrary a
                    => EncodeJson a
                    => DecodeJson a
                    => Eq a
                    => Proxy a
                    -> Effect (Exists TestTopicState)
emptyTestTopicState Proxy = do
  size <- Ref.new 1
  (serverG :: Ref (Maybe a)) <- Ref.new Nothing
  serverGReceived            <- Ref.new Nothing
  clientS                    <- Ref.new Nothing
  clientSSent                <- Ref.new Nothing
  (serverD :: Ref (Maybe a)) <- Ref.new Nothing
  serverDReceived            <- Ref.new Nothing
  (clientG :: Ref (Maybe a)) <- Ref.new Nothing
  clientGSent                <- Ref.new Nothing
  serverS                    <- Ref.new Nothing
  serverSReceived            <- Ref.new Nothing
  (clientD :: Ref (Maybe a)) <- Ref.new Nothing
  clientDSent                <- Ref.new Nothing
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

-- | State for each topic, existential in the type variable
type TestSuiteState = Ref (Map TestTopic (Exists TestTopicState))

-- | Initial state for all test topics
emptyTestSuiteState :: Effect TestSuiteState
emptyTestSuiteState = Ref.new Map.empty

-- | Control monad
type TestSuiteM a = ReaderT TestSuiteState Effect a

-- | Insert an empty state ref for the type `a`
registerTopic :: forall a
               . Arbitrary a => EncodeJson a => DecodeJson a => Eq a
              => TestTopic -> Proxy a -> TestSuiteM Unit
registerTopic topic p = do
  xsRef <- ask
  liftEffect do
    state <- emptyTestTopicState p
    void (Ref.modify (Map.insert topic state) xsRef)



-- * Error Reporting

-- | Class for checking the "okayness" of a value - useful for nested error types.
class IsOkay a where
  isOkay :: a -> Boolean
instance isOkayUnit :: IsOkay JSONUnit where
  isOkay JSONUnit = true


-- | Checked if the topic exists in the mapping
data HasTopic a
  = HasTopic a
  | NoTopic
derive instance genericHasTopic :: Generic a rep => Generic (HasTopic a) _
instance showHasTopic :: (Show a, Generic a rep) => Show (HasTopic a) where
  show = genericShow
instance isOkayHasTopic :: IsOkay a => IsOkay (HasTopic a) where
  isOkay x = case x of
    NoTopic -> false
    HasTopic y -> isOkay y

-- | Generate a new value
data GenValue a
  = DoneGenerating
  | GenValue a
derive instance genericGenValue :: Generic a rep => Generic (GenValue a) _
instance showGenValue :: (Show a, Generic a rep) => Show (GenValue a) where
  show = genericShow
instance isOkayGenValue :: IsOkay a => IsOkay (GenValue a) where
  isOkay x = case x of
    DoneGenerating -> false
    GenValue y -> isOkay y


data GotClientGenValue a
  = NoClientGenValue
  | GotClientGenValue a
derive instance genericGotClientGenValue :: Generic a rep => Generic (GotClientGenValue a) _
instance showGotClientGenValue :: (Show a, Generic a rep) => Show (GotClientGenValue a) where
  show = genericShow
instance isOkayGotClientGenValue :: IsOkay a => IsOkay (GotClientGenValue a) where
  isOkay x = case x of
    NoClientGenValue -> false
    GotClientGenValue y -> isOkay y

-- | `clientG` exists in state
data HasClientG a
  = NoClientG
  | HasClientG a
derive instance genericHasClientG :: Generic a rep => Generic (HasClientG a) _
instance showHasClientG :: (Show a, Generic a rep) => Show (HasClientG a) where
  show = genericShow
instance isOkayHasClientG :: IsOkay a => IsOkay (HasClientG a) where
  isOkay x = case x of
    NoClientG -> false
    HasClientG y -> isOkay y

-- | `serverG` exists in state
data HasServerG a
  = NoServerG
  | HasServerG a
derive instance genericHasServerG :: Generic a rep => Generic (HasServerG a) _
instance showHasServerG :: (Show a, Generic a rep) => Show (HasServerG a) where
  show = genericShow
instance isOkayHasServerG :: IsOkay a => IsOkay (HasServerG a) where
  isOkay x = case x of
    NoServerG -> false
    HasServerG y -> isOkay y

-- | `serverS` exists in state
data HasServerS a
  = NoServerS
  | HasServerS a
derive instance genericHasServerS :: Generic a rep => Generic (HasServerS a) _
instance showHasServerS :: (Show a, Generic a rep) => Show (HasServerS a) where
  show = genericShow
instance isOkayHasServerS :: IsOkay a => IsOkay (HasServerS a) where
  isOkay x = case x of
    NoServerS -> false
    HasServerS y -> isOkay y

-- | `serverD` exists in state
data HasServerD a
  = NoServerD
  | HasServerD a
derive instance genericHasServerD :: Generic a rep => Generic (HasServerD a) _
instance showHasServerD :: (Show a, Generic a rep) => Show (HasServerD a) where
  show = genericShow
instance isOkayHasServerD :: IsOkay a => IsOkay (HasServerD a) where
  isOkay x = case x of
    NoServerD -> false
    HasServerD y -> isOkay y

-- | `clientD` exists in state
data HasClientD a
  = NoClientD
  | HasClientD a
derive instance genericHasClientD :: Generic a rep => Generic (HasClientD a) _
instance showHasClientD :: (Show a, Generic a rep) => Show (HasClientD a) where
  show = genericShow
instance isOkayHasClientD :: IsOkay a => IsOkay (HasClientD a) where
  isOkay x = case x of
    NoClientD -> false
    HasClientD y -> isOkay y

-- | Deserialized the value
data DesValue a
  = CantDes String
  | DesValue a
derive instance genericDesValue :: Generic a rep => Generic (DesValue a) _
instance showDesValue :: (Show a, Generic a rep) => Show (DesValue a) where
  show = genericShow
instance isOkayDesValue :: IsOkay a => IsOkay (DesValue a) where
  isOkay x = case x of
    CantDes _ -> false
    DesValue y -> isOkay y

-- | `clientS` exists in state
data HasClientS a
  = NoClientS
  | HasClientS a
derive instance genericHasClientS :: Generic a rep => Generic (HasClientS a) _
instance showHasClientS :: (Show a, Generic a rep) => Show (HasClientS a) where
  show = genericShow
instance isOkayHasClientS :: IsOkay a => IsOkay (HasClientS a) where
  isOkay x = case x of
    NoClientS -> false
    HasClientS y -> isOkay y

-- | Type that represents the difference between two Json values
data JsonDiffBuffer
  = DiffBuffers ShowableBuffer ShowableBuffer
  | DiffBuffers' (Array Int) (Array Int)
  | DiffArrays (Array ShowableJson) (Array ShowableJson)
  | DiffObjectFields (Tuple String String) (Maybe JsonDiffBuffer)
  | DiffObjects (Array (Tuple String ShowableJson)) (Array (Tuple String ShowableJson))
derive instance genericJsonDiffbuffer :: Generic JsonDiffBuffer _
instance showJsonDiffBuffer :: Show JsonDiffBuffer where
  show = go
    where
      go x = case x of
        DiffBuffers a b -> "DiffBuffers " <> show a <> " " <> show b
        DiffBuffers' a b -> "DiffBuffers' " <> show a <> " " <> show b
        DiffArrays a b -> "DiffArrays " <> show a <> " " <> show b
        DiffObjectFields ks mx' -> "DiffObjectFields " <> show ks <> " " <> show (go <$> mx')
        DiffObjects a b -> "DiffObjects " <> show a <> " " <> show b


-- | Determine the difference between two Json values
diffJson :: Json -> Json -> Maybe JsonDiffBuffer
diffJson a b =
  case Tuple <$> Argonaut.toString a <*> Argonaut.toString b of
    Nothing ->
      let asObjects = (\a' b' -> Left $ Tuple a' b') <$> Argonaut.toObject a <*> Argonaut.toObject b
          asArrays = (\a' b' -> Right $ Tuple a' b') <$> Argonaut.toArray a <*> Argonaut.toArray b
      in  case asObjects <|> asArrays of
            Nothing
              | a == b -> Nothing
              | otherwise ->
                let viaShow = ShowableBuffer <<< fromUtf8String <<< show <<< ShowableJson
                in  Just $ DiffBuffers (viaShow a) (viaShow b)
            Just eOA -> case eOA of
              Left (Tuple ao bo) ->
                let ao' = Object.toAscUnfoldable ao
                    bo' = Object.toAscUnfoldable bo
                in  if Array.length ao' == Array.length bo'
                      then let go :: Tuple String Json -> Tuple String Json -> Maybe JsonDiffBuffer
                               go (Tuple k1 a') (Tuple k2 b')
                                 | k1 == k2 = diffJson a' b'
                                 | otherwise = Just $ DiffObjectFields (Tuple k1 k2) (diffJson a' b')
                               zss = Array.zipWith go ao' bo'
                           in  (\(First x) -> x) $ Array.foldMap First zss
                      else let toShowable (Tuple k x) = Tuple k (ShowableJson x)
                           in  Just $ DiffObjects (map toShowable ao') (map toShowable bo')
              Right (Tuple aa ba) ->
                if Array.length aa == Array.length ba
                  then let zss = Array.zipWith diffJson aa ba
                       in  (\(First x) -> x) $ Array.foldMap First zss
                  else Just (DiffArrays (map ShowableJson aa) (map ShowableJson ba))
    Just (Tuple a' b') ->
      let a'' = unsafePerformEffect (Buffer.toArray =<< (Buffer.fromString a' :: _ -> Effect Buffer) Buffer.UTF8)
          b'' = unsafePerformEffect (Buffer.toArray =<< (Buffer.fromString b' :: _ -> Effect Buffer) Buffer.UTF8)
      in  if a'' == b''
             then Nothing
             else Just (DiffBuffers' a'' b'')

-- | Simple wrapper that makes a Json value `show`able
newtype ShowableJson = ShowableJson Json
derive instance genericShowableJson :: Generic ShowableJson _
instance eqShowableJson :: Eq ShowableJson where
  eq (ShowableJson a) (ShowableJson b) =
    case diffJson a b of
      Nothing -> true
      Just _ -> false
instance showShowableJson :: Show ShowableJson where
  show (ShowableJson x) = Argonaut.stringify x
instance encodeJsonShowableJson :: EncodeJson ShowableJson where
  encodeJson (ShowableJson x) = x
instance decodeJsonShowableJson :: DecodeJson ShowableJson where
  decodeJson json = pure (ShowableJson json)

-- | Simple wrapper that makes a Buffer value `show`able
newtype ShowableBuffer = ShowableBuffer Buffer
derive instance genericShowableBuffer :: Generic ShowableBuffer _
instance eqShowableBuffer :: Eq ShowableBuffer where
  eq (ShowableBuffer x) (ShowableBuffer y) = unsafePerformEffect do
    x' <- Buffer.toArray x
    y' <- Buffer.toArray y
    pure (x' == y')
instance showShowableBuffer :: Show ShowableBuffer where
  show (ShowableBuffer x) = unsafePerformEffect $ show <$> Buffer.toArray x



-- | Ensures that `serverG` is equal to `clientS`
data ServerSerializedMatch a
  = ServerSerializedMatch a
  | ServerSerializedMismatch
    { serverG :: ShowableJson
    , clientS :: ShowableJson
    , serverGReceived :: ShowableBuffer
    , clientSSent :: ShowableBuffer
    , diff :: JsonDiffBuffer
    }
derive instance genericServerSerializedMatch :: Generic a rep => Generic (ServerSerializedMatch a) _
instance showServerSerializedMatch :: (Show a, Generic a rep) => Show (ServerSerializedMatch a) where
  show = genericShow
instance isOkayServerSerializedMatch :: IsOkay a => IsOkay (ServerSerializedMatch a) where
  isOkay x = case x of
    ServerSerializedMismatch _ -> false
    ServerSerializedMatch y -> isOkay y

-- | Ensures that `clientS` is equal to `serverD`
data ServerDeSerializedMatch a
  = ServerDeSerializedMatch a
  | ServerDeSerializedMismatch
    { clientS :: ShowableJson
    , serverD :: ShowableJson
    , clientSSent :: ShowableBuffer
    , serverDReceived :: ShowableBuffer
    }
derive instance genericServerDeSerializedMatch :: Generic a rep => Generic (ServerDeSerializedMatch a) _
instance showServerDeSerializedMatch :: (Show a, Generic a rep) => Show (ServerDeSerializedMatch a) where
  show = genericShow
instance isOkayServerDeSerializedMatch :: IsOkay a => IsOkay (ServerDeSerializedMatch a) where
  isOkay x = case x of
    ServerDeSerializedMismatch _ -> false
    ServerDeSerializedMatch y -> isOkay y

-- | Ensures that `clientG` is equal to `serverS`
data ClientSerializedMatch a
  = ClientSerializedMatch a
  | ClientSerializedMismatch
    { clientG :: ShowableJson
    , serverS :: ShowableJson
    , clientGSent :: ShowableBuffer
    , serverSReceived :: ShowableBuffer
    , diff :: JsonDiffBuffer
    }
derive instance genericClientSerializedMatch :: Generic a rep => Generic (ClientSerializedMatch a) _
instance showClientSerializedMatch :: (Show a, Generic a rep) => Show (ClientSerializedMatch a) where
  show = genericShow
instance isOkayClientSerializedMatch :: IsOkay a => IsOkay (ClientSerializedMatch a) where
  isOkay x = case x of
    ClientSerializedMismatch _ -> false
    ClientSerializedMatch y -> isOkay y

-- | Ensures that `serverS` is equal to `clientD`
data ClientDeSerializedMatch a
  = ClientDeSerializedMatch a
  | ClientDeSerializedMismatch
    { serverS :: ShowableJson
    , clientD :: ShowableJson
    , serverSReceived :: ShowableBuffer
    , clientDSent :: ShowableBuffer
    }
derive instance genericClientDeSerializedMatch :: Generic a rep => Generic (ClientDeSerializedMatch a) _
instance showClientDeSerializedMatch :: (Show a, Generic a rep) => Show (ClientDeSerializedMatch a) where
  show = genericShow
instance isOkayClientDeSerializedMatch :: IsOkay a => IsOkay (ClientDeSerializedMatch a) where
  isOkay x = case x of
    ClientDeSerializedMismatch _ -> false
    ClientDeSerializedMatch y -> isOkay y




-- * Functions

getTopicState :: TestSuiteState
              -> TestTopic
              -> Effect (HasTopic (Exists TestTopicState))
getTopicState xsRef topic = do
  xs <- Ref.read xsRef
  case Map.lookup topic xs of
    Nothing -> pure NoTopic
    Just x -> pure (HasTopic x)


generateValue :: Exists TestTopicState
              -> TestTopic
              -> Int
              -> Effect (GenValue ClientToServer)
generateValue ex topic maxSize =
  let go :: forall a. TestTopicState a -> Effect (GenValue ClientToServer)
      go (TestTopicState {size,generate,serialize,clientG}) = do
        s <- Ref.read size
        if s >= maxSize
          then pure DoneGenerating
          else do
            g <- randomSeed
            let val = evalState (unGen generate) {newSeed: g, size: s}
            void (Ref.modify (\x -> x + 1) size)
            Ref.write (Just val) clientG
            pure $ GenValue $ ClientToServer topic GeneratedInput $ ShowableJson $ serialize val
  in  runExists go ex


gotServerGenValue :: Exists TestTopicState
                  -> Json
                  -> Effect (DesValue JSONUnit)
gotServerGenValue ex value =
  let go :: forall a. TestTopicState a -> Effect (DesValue JSONUnit)
      go (TestTopicState {deserialize,serverG}) = case deserialize value of
        Left e -> pure (CantDes e)
        Right y -> do
          Ref.write (Just y) serverG
          pure (DesValue JSONUnit)
  in  runExists go ex


serializeValueServerOrigin :: Exists TestTopicState
                           -> TestTopic
                           -> Effect (HasServerG ClientToServer)
serializeValueServerOrigin ex topic =
  let go :: forall a. TestTopicState a -> Effect (HasServerG ClientToServer)
      go (TestTopicState {serialize,serverG,clientS}) = do
        mX <- Ref.read serverG
        case mX of
          Nothing -> pure NoServerG
          Just x -> map HasServerG $ do
            let val = serialize x
            Ref.write (Just val) clientS
            pure $ ClientToServer topic Serialized $ ShowableJson val
  in  runExists go ex


gotServerSerialize :: Exists TestTopicState
                   -> Json
                   -> Effect JSONUnit
gotServerSerialize ex value =
  let go :: forall a. TestTopicState a -> Effect JSONUnit
      go (TestTopicState {deserialize,serverS}) = do
        Ref.write (Just value) serverS
        pure JSONUnit
  in  runExists go ex


deserializeValueServerOrigin :: Exists TestTopicState
                             -> TestTopic
                             -> Effect (HasServerS (DesValue ClientToServer))
deserializeValueServerOrigin ex topic =
  let go :: forall a. TestTopicState a -> Effect (HasServerS (DesValue ClientToServer))
      go (TestTopicState {deserialize,serverS,clientD,serialize}) = do
        mX <- Ref.read serverS
        case mX of
          Nothing -> pure NoServerS
          Just x -> map HasServerS $ case deserialize x of
            Left e -> pure (CantDes e)
            Right y -> do
              Ref.write (Just y) clientD
              pure $ DesValue $ ClientToServer topic DeSerialized $ ShowableJson $ serialize y
  in  runExists go ex


gotServerDeSerialize :: Exists TestTopicState
                     -> Json
                     -> Effect (DesValue JSONUnit)
gotServerDeSerialize ex value =
  let go :: forall a. TestTopicState a -> Effect (DesValue JSONUnit)
      go (TestTopicState {deserialize,serverD}) = case deserialize value of
        Left e -> pure (CantDes e)
        Right y -> do
          Ref.write (Just y) serverD
          pure (DesValue JSONUnit)
  in  runExists go ex


verify :: Exists TestTopicState
       -> Effect
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
                                ( ServerDeSerializedMatch JSONUnit))))))))))))
verify ex =
  let go :: forall a. TestTopicState a -> Effect (HasClientG (HasServerS (ClientSerializedMatch (HasClientD (DesValue (ClientDeSerializedMatch (HasServerG (HasClientS (ServerSerializedMatch (HasServerD (DesValue (ServerDeSerializedMatch JSONUnit))))))))))))
      go (TestTopicState
        { serialize
        , deserialize
        , eq
        , clientG
        , serverS
        , clientD
        , serverG
        , clientS
        , serverD
        }) = do
        let clientSMatch :: forall b
                          . (Json -> Effect b)
                         -> Effect
                            (HasClientG
                              (HasServerS
                                (ClientSerializedMatch b)))
            clientSMatch x = do
              mClientG <- Ref.read clientG
              case mClientG of
                Nothing -> pure NoClientG
                Just clientG' -> map HasClientG $ do
                  mServerS <- Ref.read serverS
                  case mServerS of
                    Nothing -> pure NoServerS
                    Just serverS' -> map HasServerS $
                      let clientG'' = serialize clientG'
                      in  case diffJson clientG'' serverS' of
                        Just diff ->
                          pure $ ClientSerializedMismatch
                            { clientG: ShowableJson clientG''
                            , serverS: ShowableJson serverS'
                            , clientGSent: ShowableBuffer $ fromUtf8String $ show $ ShowableJson clientG''
                            , serverSReceived: ShowableBuffer $ fromUtf8String $ show $ ShowableJson serverS'
                            , diff
                            }
                        Nothing -> ClientSerializedMatch <$> x serverS'

            clientDMatch :: forall b
                          . Effect b -> Json
                         -> Effect
                            (HasClientD
                              (DesValue
                                (ClientDeSerializedMatch b)))
            clientDMatch x serverS' = do
              mClientD <- Ref.read clientD
              case mClientD of
                Nothing -> pure NoClientD
                Just clientD' -> map HasClientD $ do
                  case deserialize serverS' of
                    Left e -> pure (CantDes e)
                    Right serverS'' ->
                      if not (eq serverS'' clientD')
                        then do
                          let clientD'' = serialize clientD'
                          pure $ DesValue $ ClientDeSerializedMismatch
                            { serverS: ShowableJson serverS'
                            , clientD: ShowableJson clientD''
                            , serverSReceived: ShowableBuffer $ fromUtf8String $ show $ ShowableJson serverS'
                            , clientDSent: ShowableBuffer $ fromUtf8String $ show $ ShowableJson clientD''
                            }
                        else (DesValue <<< ClientDeSerializedMatch) <$> x

            serverSMatch :: forall b
                          . (Json -> Effect b)
                         -> Effect
                            (HasServerG
                              (HasClientS
                                (ServerSerializedMatch b)))
            serverSMatch x = do
              mServerG <- Ref.read serverG
              case mServerG of
                Nothing -> pure NoServerG
                Just serverG' -> map HasServerG $ do
                  mClientS <- Ref.read clientS
                  case mClientS of
                    Nothing -> pure NoClientS
                    Just clientS' -> map HasClientS $
                      let serverG'' = serialize serverG'
                      in  case diffJson serverG'' clientS' of
                        Just diff ->
                          pure $ ServerSerializedMismatch
                            { serverG: ShowableJson serverG''
                            , clientS: ShowableJson clientS'
                            , serverGReceived: ShowableBuffer $ fromUtf8String $ show $ ShowableJson serverG''
                            , clientSSent: ShowableBuffer $ fromUtf8String $ show $ ShowableJson clientS'
                            , diff
                            }
                        Nothing -> ServerSerializedMatch <$> x clientS'

            serverDMatch :: Json
                         -> Effect
                            (HasServerD
                              (DesValue
                                (ServerDeSerializedMatch JSONUnit)))
            serverDMatch clientS' = do
              mServerD <- Ref.read serverD
              case mServerD of
                Nothing -> pure NoServerD
                Just serverD' -> map HasServerD $ do
                  case deserialize clientS' of
                    Left e -> pure (CantDes e)
                    Right clientS'' ->
                      if not (eq clientS'' serverD')
                        then do
                          let serverD'' = serialize serverD'
                          pure $ DesValue $ ServerDeSerializedMismatch
                            { clientS: ShowableJson clientS'
                            , serverD: ShowableJson serverD''
                            , clientSSent: ShowableBuffer $ fromUtf8String $ show $ ShowableJson clientS'
                            , serverDReceived: ShowableBuffer $ fromUtf8String $ show $ ShowableJson serverD''
                            }
                        else pure $ DesValue $ ServerDeSerializedMatch JSONUnit
        clientSMatch $ clientDMatch $ serverSMatch serverDMatch
  in  runExists go ex




foreign import toHexString :: Buffer -> String
foreign import toUtf8String :: Buffer -> String
foreign import fromHexString :: String -> Buffer
foreign import fromUtf8String :: String -> Buffer


