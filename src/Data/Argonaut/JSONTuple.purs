module Data.Argonaut.JSONTuple where

import Prelude
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (~>), jsonEmptyObject, (:=), (.?))
import Test.QuickCheck (class Arbitrary, arbitrary)


data JSONTuple a b = JSONTuple a b

derive instance genericJSONTuple :: (Generic a, Generic b) => Generic (JSONTuple a b)

instance eqJSONTuple :: (Generic a, Generic b) => Eq (JSONTuple a b) where
  eq = gEq

instance showJSONTuple :: (Generic a, Generic b) => Show (JSONTuple a b) where
  show = gShow

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
