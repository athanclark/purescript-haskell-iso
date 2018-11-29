
module Data.Argonaut.JSONMaybe where

import Prelude
import Data.NonEmpty (NonEmpty (..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Array (unsafeIndex, length) as Array
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail)
import Control.Alternative ((<|>))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)
import Partial.Unsafe (unsafePartial)


data JSONMaybe a
  = JSONNothing
  | JSONJust a

derive instance genericJSONMaybe :: (Generic a rep1) => Generic (JSONMaybe a) _
instance eqJSONMaybe :: (Eq a, Generic a rep1) => Eq (JSONMaybe a) where
  eq = genericEq
instance showJSONMaybe :: (Show a, Generic a rep1) => Show (JSONMaybe a) where
  show = genericShow

instance arbitraryJSONMaybe :: (Arbitrary a) => Arbitrary (JSONMaybe a) where
  arbitrary = oneOf $ NonEmpty
    (pure JSONNothing)
    [JSONJust <$> arbitrary]

instance encodeJsonJSONMaybe :: (EncodeJson a) => EncodeJson (JSONMaybe a) where
  encodeJson x = case x of
    JSONNothing -> encodeJson ""
    JSONJust y -> encodeJson [y]

instance decodeJsonJSONMaybe :: (DecodeJson a) => DecodeJson (JSONMaybe a) where
  decodeJson json = do
    let str = do
          s <- decodeJson json
          if s == "" then pure JSONNothing else fail "JSONMaybe"
        arr = do
          a <- decodeJson json
          if Array.length a == 1
             then pure $ unsafePartial $ JSONJust $ Array.unsafeIndex a 0
             else fail "JSONMaybe"
    str <|> arr
