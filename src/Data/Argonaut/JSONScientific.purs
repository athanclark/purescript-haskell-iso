module Data.Argonaut.JSONScientific where

import Prelude
import Data.BigNumber (BigNumber, parseBigNumber, toExponential)
import Data.Either (Either (..))
import Data.NonEmpty (NonEmpty (..))
import Data.Enum (enumFromTo)
import Data.Generic.Rep (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail)
import Data.String (length) as String
import Data.String.Yarn (fromChars) as String
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (arrayOf1, arrayOf, elements)
import Partial.Unsafe (unsafePartial)


newtype JSONScientific = JSONScientific BigNumber
derive instance genericJSONScientific :: Generic JSONScientific _
derive newtype instance eqJSONScientific :: Eq JSONScientific
derive newtype instance showJSONScientific :: Show JSONScientific

instance encodeJsonJSONScientific :: EncodeJson JSONScientific where
  encodeJson (JSONScientific x) = encodeJson (toExponential x)

instance decodeJsonJSONScientific :: DecodeJson JSONScientific where
  decodeJson json = do
    s <- decodeJson json
    case parseBigNumber s of
      Left e -> fail (show e)
      Right x -> pure (JSONScientific x)

instance arbitraryJSONScientific :: Arbitrary JSONScientific where
  arbitrary = do
    n <- arbitrary
    s <- String.fromChars <$> arrayOf1 (elements $ NonEmpty '0' $ enumFromTo '1' '9')
    p <- String.fromChars <$> arrayOf (elements $ NonEmpty '0' $ enumFromTo '1' '9')
    unsafePartial $ case parseBigNumber ((if n then "-" else "") <> s <> (if String.length p == 0 then "" else "." <> p)) of
      Right x -> pure (JSONScientific x)
    -- (x :: Number) <- arbitrary
    -- unsafePartial $ case Number.fromString $ trimDecimal $ show x of
    --   Just y -> pure (JSONScientific y)
