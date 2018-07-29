module Data.Argonaut.JSONUnit where

import Prelude
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail)
import Test.QuickCheck (class Arbitrary)


data JSONUnit = JSONUnit

derive instance genericJSONUnit :: Generic JSONUnit

instance eqJSONUnit :: Eq JSONUnit where
  eq = gEq

instance showJSONUnit :: Show JSONUnit where
  show = gShow

instance arbitraryJSONUnit :: Arbitrary JSONUnit where
  arbitrary = pure JSONUnit

instance encodeJsonJSONUnit :: EncodeJson JSONUnit where
  encodeJson JSONUnit = encodeJson ""

instance decodeJsonJSONUnit :: DecodeJson JSONUnit where
  decodeJson json = do
    s <- decodeJson json
    if s == ""
      then pure JSONUnit
      else fail "Not a JSONUnit"
