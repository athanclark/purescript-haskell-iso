module Data.Argonaut.JSONUnit where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail)
import Test.QuickCheck (class Arbitrary)


data JSONUnit = JSONUnit

derive instance genericJSONUnit :: Generic JSONUnit _
instance eqJSONUnit :: Eq JSONUnit where
  eq = genericEq

instance showJSONUnit :: Show JSONUnit where
  show = genericShow

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
