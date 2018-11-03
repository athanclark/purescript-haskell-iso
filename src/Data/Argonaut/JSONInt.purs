module Data.Argonaut.JSONInt where


import Prelude
import Data.Generic.Rep (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Test.QuickCheck (class Arbitrary)


newtype JSONInt = JSONInt Int
derive instance genericJSONInt :: Generic JSONInt _
derive newtype instance eqJSONInt :: Eq JSONInt
derive newtype instance showJSONInt :: Show JSONInt
derive newtype instance encodeJsonJSONInt :: EncodeJson JSONInt
derive newtype instance decodeJsonJSONInt :: DecodeJson JSONInt
derive newtype instance arbitraryJSONInt :: Arbitrary JSONInt
