module Test.Main where

import Test.Serialization (startClient)
import Test.Serialization.Types
  ( TestSuiteM, TestTopic (..), MsgType, ClientToServer, ServerToClient
  , registerTopic)
import Data.Argonaut.JSONUnit (JSONUnit)
import Data.Argonaut.JSONEither (JSONEither)
import Data.Argonaut.JSONTuple (JSONTuple)
import Data.Argonaut.JSONDate (JSONDate)
import Data.Argonaut.JSONDateTime (JSONDateTime)
import Data.Argonaut.JSONString (JSONString)
import Data.Argonaut.JSONEmailAddress (JSONEmailAddress)
import Data.Argonaut.JSONInt (JSONInt)
import Data.Argonaut.JSONInteger (JSONInteger)
import Data.Argonaut.JSONScientific (JSONScientific)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.These (These (..))
import Data.Tuple (Tuple (..))
import Data.String.NonEmpty as String
import URI (Authority (..), Host (NameAddress), UserInfo)
import URI.HostPortPair (HostPortPair)
import URI.Host.RegName as RegName
import URI.Port as Port
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Type.Proxy (Proxy (..))
-- import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Console (CONSOLE, log)
-- import Control.Monad.Eff.Ref (REF)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck, Result (..))
import Partial.Unsafe (unsafePartial)




main :: Effect Unit
main = do
  log "Starting tests..."
  -- quickCheck (jsonIso :: JSONEither JSONString JSONString -> _)
  startClient
    { controlHost:
      let uri :: Authority UserInfo (HostPortPair Host Port.Port)
          uri = Authority Nothing $ Just $ Both
                (NameAddress $ RegName.unsafeFromString $ unsafePartial $ String.unsafeFromString "127.0.0.1")
                (Port.unsafeFromInt 5561)
      in  uri
    , testSuite: tests
    , maxSize: 200
    }



tests :: TestSuiteM Unit
tests = do
  registerTopic (TestTopic "TestTopic") (Proxy :: Proxy TestTopic)
  registerTopic (TestTopic "MsgType") (Proxy :: Proxy MsgType)
  registerTopic (TestTopic "ClientToServer") (Proxy :: Proxy ClientToServer)
  registerTopic (TestTopic "ServerToClient") (Proxy :: Proxy ServerToClient)
  registerTopic (TestTopic "JSONUnit") (Proxy :: Proxy JSONUnit)
  registerTopic (TestTopic "JSONEither") (Proxy :: Proxy (JSONEither JSONUnit JSONUnit))
  registerTopic (TestTopic "JSONTuple") (Proxy :: Proxy (JSONTuple JSONUnit JSONUnit))
  registerTopic (TestTopic "JSONDate") (Proxy :: Proxy JSONDate)
  registerTopic (TestTopic "JSONDateTime") (Proxy :: Proxy JSONDateTime)
  registerTopic (TestTopic "JSONString") (Proxy :: Proxy JSONString)
  registerTopic (TestTopic "JSONEmailAddress") (Proxy :: Proxy JSONEmailAddress)
  registerTopic (TestTopic "JSONInt") (Proxy :: Proxy JSONInt)
  registerTopic (TestTopic "JSONInteger") (Proxy :: Proxy JSONInteger)
  registerTopic (TestTopic "JSONScientific") (Proxy :: Proxy JSONScientific)



jsonIso :: forall a
         . EncodeJson a
         => DecodeJson a
         => Eq a
         => Show a
         => a -> Result
jsonIso x = case decodeJson (encodeJson x) of
  Left e -> Failed e
  Right y
    | y == x -> Success
    | otherwise -> Failed $ "Mismatch: " <> show x <> ", " <> show y
