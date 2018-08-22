module Test.Main where

import Test.Serialization (startClient)
import Test.Serialization.Types
  ( TestSuiteM, TestTopic (..), ChannelMsg, ClientToServer, ServerToClient
  , registerTopic)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.URI (Authority (..), Host (NameAddress), Port (..))
import Type.Proxy (Proxy (..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF)




main :: Eff _ Unit
main = do
  log "Starting tests..."
  startClient
    { controlHost: Authority Nothing [Tuple (NameAddress "localhost") (Just (Port 5561))]
    , testSuite: tests
    }



tests :: forall eff
       . TestSuiteM (ref :: REF | eff) Unit
tests = do
  registerTopic (TestTopic "ChannelMsg") (Proxy :: Proxy ChannelMsg)
  registerTopic (TestTopic "ClientToServer") (Proxy :: Proxy ClientToServer)
  registerTopic (TestTopic "ServerToClient") (Proxy :: Proxy ServerToClient)
