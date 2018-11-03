module Test.Serialization.Types where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
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
import Type.Proxy (Proxy (..))
import Control.Alternative ((<|>))
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (evalState)
-- import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Class (liftEff)
-- import Control.Monad.Eff.Ref (REF, Ref, Ref.new, Ref.read, Ref.write, Ref.modify)
-- import Control.Monad.Eff.Random (RANDOM)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, unGen, oneOf, arrayOf1, elements)
import Random.LCG (randomSeed)
import Node.Buffer (Buffer)



-- * Network Messages

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


arbitraryJson :: Gen Json
arbitraryJson = oneOf $ NonEmpty
  ((encodeJson :: Int -> Json) <$> arbitrary)
  []


data ClientToServer
  = GetTopics
  | ClientToServer TestTopic MsgType ShowableJson
  | ClientToServerBadParse String
  | Finished TestTopic

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


data ServerToClient
  = TopicsAvailable (Set TestTopic)
  | ServerToClient TestTopic MsgType ShowableJson
  | ServerToClientBadParse String
  | Continue TestTopic

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
  serverGReceived <- Ref.new Nothing
  clientS <- Ref.new Nothing
  clientSSent <- Ref.new Nothing
  (serverD :: Ref (Maybe a)) <- Ref.new Nothing
  serverDReceived <- Ref.new Nothing
  (clientG :: Ref (Maybe a)) <- Ref.new Nothing
  clientGSent <- Ref.new Nothing
  serverS <- Ref.new Nothing
  serverSReceived <- Ref.new Nothing
  (clientD :: Ref (Maybe a)) <- Ref.new Nothing
  clientDSent <- Ref.new Nothing
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

emptyTestSuiteState :: Effect TestSuiteState
emptyTestSuiteState = Ref.new Map.empty


type TestSuiteM a = ReaderT TestSuiteState Effect a


registerTopic :: forall a
               . Arbitrary a => EncodeJson a => DecodeJson a => Eq a
              => TestTopic -> Proxy a -> TestSuiteM Unit
registerTopic topic p = do
  xsRef <- ask
  liftEffect $ do
    state <- emptyTestTopicState p
    void (Ref.modify (Map.insert topic state) xsRef)



-- * Error Reporting

class IsOkay a where
  isOkay :: a -> Boolean

instance isOkayUnit :: IsOkay JSONUnit where
  isOkay JSONUnit = true


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


newtype ShowableJson = ShowableJson Json

derive instance genericShowableJson :: Generic ShowableJson _
derive newtype instance eqShowableJson :: Eq ShowableJson
instance showShowableJson :: Show ShowableJson where
  show (ShowableJson x) = case Argonaut.toString x of
    Nothing -> "bad json"
    Just y -> y
instance encodeJsonShowableJson :: EncodeJson ShowableJson where
  encodeJson (ShowableJson x) = x
instance decodeJsonShowableJson :: DecodeJson ShowableJson where
  decodeJson json = pure (ShowableJson json)


data ServerSerializedMatch a
  = ServerSerializedMatch a
  | ServerSerializedMismatch
    { serverG :: ShowableJson
    , clientS :: ShowableJson
    , serverGReceived :: Buffer
    , clientSSent :: Buffer
    }

derive instance genericServerSerializedMatch :: Generic a rep => Generic (ServerSerializedMatch a) _
instance showServerSerializedMatch :: (Show a, Generic a rep) => Show (ServerSerializedMatch a) where
  show = genericShow

instance isOkayServerSerializedMatch :: IsOkay a => IsOkay (ServerSerializedMatch a) where
  isOkay x = case x of
    ServerSerializedMismatch _ -> false
    ServerSerializedMatch y -> isOkay y


data ServerDeSerializedMatch a
  = ServerDeSerializedMatch a
  | ServerDeSerializedMismatch
    { clientS :: ShowableJson
    , serverD :: ShowableJson
    , clientSSent :: Buffer
    , serverDReceived :: Buffer
    }

derive instance genericServerDeSerializedMatch :: Generic a rep => Generic (ServerDeSerializedMatch a) _
instance showServerDeSerializedMatch :: (Show a, Generic a rep) => Show (ServerDeSerializedMatch a) where
  show = genericShow

instance isOkayServerDeSerializedMatch :: IsOkay a => IsOkay (ServerDeSerializedMatch a) where
  isOkay x = case x of
    ServerDeSerializedMismatch _ -> false
    ServerDeSerializedMatch y -> isOkay y


data ClientSerializedMatch a
  = ClientSerializedMatch a
  | ClientSerializedMismatch
    { clientG :: ShowableJson
    , serverS :: ShowableJson
    , clientGSent :: Buffer
    , serverSReceived :: Buffer
    }

derive instance genericClientSerializedMatch :: Generic a rep => Generic (ClientSerializedMatch a) _
instance showClientSerializedMatch :: (Show a, Generic a rep) => Show (ClientSerializedMatch a) where
  show = genericShow

instance isOkayClientSerializedMatch :: IsOkay a => IsOkay (ClientSerializedMatch a) where
  isOkay x = case x of
    ClientSerializedMismatch _ -> false
    ClientSerializedMatch y -> isOkay y


data ClientDeSerializedMatch a
  = ClientDeSerializedMatch a
  | ClientDeSerializedMismatch
    { serverS :: ShowableJson
    , clientD :: ShowableJson
    , serverSReceived :: Buffer
    , clientDSent :: Buffer
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


verify :: forall eff
        . Exists TestTopicState
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
  let go :: forall a. TestTopicState a -> Effect _
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
        let clientSMatch :: (Json -> Effect _)
                         -> Effect
                            (HasClientG
                              (HasServerS
                                (ClientSerializedMatch _)))
            clientSMatch x = do
              mClientG <- Ref.read clientG
              case mClientG of
                Nothing -> pure NoClientG
                Just clientG' -> map HasClientG $ do
                  mServerS <- Ref.read serverS
                  case mServerS of
                    Nothing -> pure NoServerS
                    Just serverS' -> map HasServerS $ do
                      let clientG'' = serialize clientG'
                      if  clientG'' /= serverS'
                        then pure $ ClientSerializedMismatch
                              { clientG: ShowableJson clientG''
                              , serverS: ShowableJson serverS'
                              , clientGSent: fromUtf8String $ show $ ShowableJson clientG''
                              , serverSReceived: fromUtf8String $ show $ ShowableJson serverS'
                              }
                        else ClientSerializedMatch <$> x serverS'

            clientDMatch :: Effect _ -> Json
                         -> Effect
                            (HasClientD
                              (DesValue
                                (ClientDeSerializedMatch _)))
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
                            , serverSReceived: fromUtf8String $ show $ ShowableJson serverS'
                            , clientDSent: fromUtf8String $ show $ ShowableJson clientD''
                            }
                        else (DesValue <<< ClientDeSerializedMatch) <$> x

            serverSMatch :: (Json -> Effect _)
                         -> Effect
                            (HasServerG
                              (HasClientS
                                (ServerSerializedMatch _)))
            serverSMatch x = do
              mServerG <- Ref.read serverG
              case mServerG of
                Nothing -> pure NoServerG
                Just serverG' -> map HasServerG $ do
                  mClientS <- Ref.read clientS
                  case mClientS of
                    Nothing -> pure NoClientS
                    Just clientS' -> map HasClientS $ do
                      let serverG'' = serialize serverG'
                      if  serverG'' /= clientS'
                        then pure $ ServerSerializedMismatch
                                { serverG: ShowableJson serverG''
                                , clientS: ShowableJson clientS'
                                , serverGReceived: fromUtf8String $ show $ ShowableJson serverG''
                                , clientSSent: fromUtf8String $ show $ ShowableJson clientS'
                                }
                        else ServerSerializedMatch <$> x clientS'

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
                            , clientSSent: fromUtf8String $ show $ ShowableJson clientS'
                            , serverDReceived: fromUtf8String $ show $ ShowableJson serverD''
                            }
                        else pure $ DesValue $ ServerDeSerializedMatch JSONUnit
        clientSMatch $ clientDMatch $ serverSMatch serverDMatch
  in  runExists go ex




foreign import toHexString :: Buffer -> String
foreign import toUtf8String :: Buffer -> String
foreign import fromHexString :: String -> Buffer
foreign import fromUtf8String :: String -> Buffer


