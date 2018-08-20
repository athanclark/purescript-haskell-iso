module Test.Serialization where

import Test.Serialization.Types
  ( TestSuiteM, TestSuiteState, emptyTestSuiteState
  , ClientToServer (..), ServerToClient (..), ChannelMsg (..))

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.URI (URI (..), Scheme (..), HierarchicalPart (..), Authority)
import Data.URI.URI as URI
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Control.Monad.Reader (runReaderT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Aff (runAff_)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, throw)
import ZeroMQ
  (ZEROMQ, ZMQIdent, socket, router, dealer, connect, sendJson, readJson)
import Node.Buffer (BUFFER)


type ClientParams (eff :: # Effect) =
  { controlHost :: Authority
  , testSuite :: TestSuiteM eff Unit
  }



type Effects eff =
  ( ref :: REF
  , zeromq :: ZEROMQ
  , exception :: EXCEPTION
  , buffer :: BUFFER
  | eff)


startClient :: forall eff
             . ClientParams (Effects eff)
            -> Eff (Effects eff) Unit
startClient {controlHost,testSuite} = do
  client <- socket dealer router
  connect client $ URI.print $
    URI
      (Just (Scheme "tcp"))
      (HierarchicalPart (Just controlHost) Nothing) Nothing Nothing

  suite <- do
    suiteStateRef <- emptyTestSuiteState
    runReaderT testSuite suiteStateRef
    readRef suiteStateRef

  let topics = Set.fromFoldable (Map.keys suite)

      resolve eX = case eX of
        Left e -> throwException e
        Right _ -> pure unit

  runAff_ resolve $ do
    liftEff $ sendJson unit client GetTopics
    mX <- readJson client
    case mX of
      Nothing -> liftEff $ throw "No receipt"
      Just {msg} -> case msg of
        TopicsAvailable ts
          | ts == topics -> pure unit
          | otherwise -> liftEff $ throw $ "Mismatched topics: "
                      <> show ts <> ", on client: " <> show topics
        _ -> liftEff $ throw $ "Incorrect timing?: " -- <> show msg
    pure unit
