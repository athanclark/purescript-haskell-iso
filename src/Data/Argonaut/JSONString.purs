module Data.Argonaut.JSONString where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Test.QuickCheck (class Arbitrary)


newtype JSONString = JSONString String

derive instance genericJSONString :: Generic JSONString _
derive newtype instance showJSONString :: Show JSONString
derive newtype instance eqJSONString :: Eq JSONString
derive newtype instance ordJSONString :: Ord JSONString
derive newtype instance encodeJsonJSONString :: EncodeJson JSONString
derive newtype instance decodeJsonJSONString :: DecodeJson JSONString
derive newtype instance arbitraryJSONString :: Arbitrary JSONString


getJSONString :: JSONString -> String
getJSONString (JSONString x) = x
