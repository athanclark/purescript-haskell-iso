
module Data.Argonaut.JSONEither where

import Prelude
import Data.NonEmpty (NonEmpty (..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (~>), jsonEmptyObject, (:=), (.?))
import Control.Alternative ((<|>))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)


data JSONEither a b
  = JSONLeft a
  | JSONRight b

derive instance genericJSONEither :: (Generic a rep1, Generic b rep2) => Generic (JSONEither a b) _
instance eqJSONEither :: (Eq a, Eq b, Generic a rep1, Generic b rep2) => Eq (JSONEither a b) where
  eq = genericEq
instance showJSONEither :: (Show a, Show b, Generic a rep1, Generic b rep2) => Show (JSONEither a b) where
  show = genericShow

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
