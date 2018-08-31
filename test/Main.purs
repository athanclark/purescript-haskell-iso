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

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Tuple (Tuple (..))
import Data.URI (Authority (..), Host (NameAddress), Port (..))
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Type.Proxy (Proxy (..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF)
import Test.QuickCheck (quickCheck, Result (..))




main :: Eff _ Unit
main = do
  log "Starting tests..."
  -- quickCheck (jsonIso :: JSONEither JSONString JSONString -> _)
  startClient
    { controlHost: Authority Nothing [Tuple (NameAddress "localhost") (Just (Port 5561))]
    , testSuite: tests
    }



tests :: forall eff
       . TestSuiteM (ref :: REF | eff) Unit
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
