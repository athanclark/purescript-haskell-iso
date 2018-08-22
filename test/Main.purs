module Test.Main where

import Test.Serialization ()
import Test.Serialization.Types
  (TestSuiteM, ChannelMsg, registerTopic)

import Prelude
import Type.Proxy (Proxy (..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF)




main :: Eff _ Unit
main = do
  log "You should add some tests."



tests :: forall eff
       . TestSuiteM (ref :: REF | eff) Unit
tests = do
  registerTopic "ChannelMsg" (Proxy :: Proxy ChannelMsg)
