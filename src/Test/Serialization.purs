module Test.Serialization where

import Test.Serialization.Types
  ( TestSuiteM, TestSuiteState, emptyTestSuiteState
  , ClientToServer (..), ServerToClient (..), ChannelMsg (..)
  , HasTopic (..), GenValue (..)
  , generateValue
  )

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.URI (URI (..), Scheme (..), HierarchicalPart (..), Authority)
import Data.URI.URI as URI
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Data.Argonaut (encodeJson)
import Data.Foldable (for_)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Aff (Aff, runAff_, forkAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, throw)
import Control.Monad.Eff.Random (RANDOM)
import ZeroMQ
  ( ZEROMQ, socket, router, dealer, connect, sendJson, readJson
  , Socket, Router, Dealer, Connected)
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
  , random :: RANDOM
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

  suiteStateRef <- emptyTestSuiteState
  topics <- do
    runReaderT testSuite suiteStateRef
    Set.fromFoldable <<< Map.keys <$> readRef suiteStateRef

  let resolve eX = case eX of
        Left e -> throwException e
        Right _ -> pure unit

  runAff_ resolve $ do
    liftEff $ sendJson unit client GetTopics
    mX <- readJson client
    case mX of
      Nothing -> liftEff $ throw "No receipt"
      Just {msg} -> case msg of
        TopicsAvailable ts
          | ts == topics -> do
            -- spark a listening async thread
            receiver <- forkAff (receiveClient suiteStateRef client)
            for_ ts \t -> do
              mX' <- liftEff $ generateValue suiteStateRef t
              case mX' of
                HasTopic (GenValue outgoing) ->
                  liftEff $ sendJson unit client outgoing
                _ -> liftEff $ throw $ "Can't generate initial value? " <> t
            pure unit
          | otherwise -> liftEff $ throw $ "Mismatched topics: "
                      <> show ts <> ", on client: " <> show topics
        _ -> liftEff $ throw $ "Incorrect timing?: " <> show (encodeJson msg)


receiveClient :: forall eff
               . TestSuiteState
              -> Socket Dealer Router Connected
              -> Aff (Effects eff) Unit
receiveClient suiteStateRef client = forever $ do
  mX <- readJson client
  case mX of
    Nothing -> liftEff $ throw "No value?"
    Just {msg} -> case msg of
      ServerToClientBadParse e -> liftEff $ throw $ "Bad parse: " <> e
      ServerToClient _ -> pure unit
      _ -> pure unit
  pure unit
