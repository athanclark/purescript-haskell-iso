
module Data.Argonaut.JSONEither where

import Prelude
import Data.NonEmpty (NonEmpty (..))
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (~>), jsonEmptyObject, (:=), (.?))
import Control.Alternative ((<|>))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)


data JSONEither a b
  = JSONLeft a
  | JSONRight b

derive instance genericJSONEither :: (Generic a, Generic b) => Generic (JSONEither a b)

instance eqJSONEither :: (Generic a, Generic b) => Eq (JSONEither a b) where
  eq = gEq

instance showJSONEither :: (Generic a, Generic b) => Show (JSONEither a b) where
  show = gShow

instance arbitraryJSONEither :: (Arbitrary a, Arbitrary b) => Arbitrary (JSONEither a b) where
  arbitrary = oneOf $ NonEmpty
    (JSONLeft <$> arbitrary)
    [JSONRight <$> arbitrary]

instance encodeJsonJSONEither :: (EncodeJson a, EncodeJson b) => EncodeJson (JSONEither a b) where
  encodeJson x = case x of
    JSONLeft a -> "e" := a ~> jsonEmptyObject
    JSONRight b -> "x" := b ~> jsonEmptyObject

instance decodeJsonJSONEither :: (DecodeJson a, DecodeJson b) => DecodeJson (JSONEither a b) where
  decodeJson json = do
    o <- decodeJson json
    let e = JSONLeft <$> o .? "e"
        x = JSONRight <$> o .? "x"
    e <|> x
