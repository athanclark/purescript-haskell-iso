module Data.Argonaut.JSONVoid where

import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, fail)



data JSONVoid

instance encodeJsonJSONVoid :: EncodeJson JSONVoid where
  encodeJson _ = encodeJson ""

instance decodeJsonJSONVoid :: DecodeJson JSONVoid where
  decodeJson _ = fail "JSONVoid"
