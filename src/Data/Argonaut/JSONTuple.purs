module Data.Argonaut.JSONTuple where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (~>), jsonEmptyObject, (:=), (.?))
import Test.QuickCheck (class Arbitrary, arbitrary)


data JSONTuple a b = JSONTuple a b

derive instance genericJSONTuple :: (Generic a rep1, Generic b rep2) => Generic (JSONTuple a b) _
instance eqJSONTuple :: (Eq a, Eq b, Generic a rep1, Generic b rep2) => Eq (JSONTuple a b) where
  eq = genericEq
instance showJSONTuple :: (Show a, Show b, Generic a rep1, Generic b rep2) => Show (JSONTuple a b) where
  show = genericShow

instance arbitraryJSONTuple :: (Arbitrary a, Arbitrary b) => Arbitrary (JSONTuple a b) where
  arbitrary = JSONTuple <$> arbitrary <*> arbitrary

instance encodeJsonJSONTuple :: (EncodeJson a, EncodeJson b) => EncodeJson (JSONTuple a b) where
  encodeJson (JSONTuple a b)
    =  "l" := a
    ~> "r" := b
    ~> jsonEmptyObject

instance decodeJsonJSONTuple :: (DecodeJson a, DecodeJson b) => DecodeJson (JSONTuple a b) where
  decodeJson json = do
    o <- decodeJson json
    a <- o .? "l"
    b <- o .? "r"
    pure (JSONTuple a b)
