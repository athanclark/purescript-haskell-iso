module Data.Argonaut.JSONInteger where

import Prelude
import Data.BigNumber (BigNumber, parseBigNumber, toExponential, randomBigNumber, intValue)
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.NonEmpty (NonEmpty (..))
import Data.Enum (enumFromTo)
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail)
import Data.Number as Number
import Data.String.Yarn as String
import Data.Array as Array
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (arrayOf1, elements)
import Partial.Unsafe (unsafePartial)


newtype JSONInteger = JSONInteger BigNumber
derive instance genericJSONInteger :: Generic JSONInteger
derive newtype instance eqJSONInteger :: Eq JSONInteger
derive newtype instance showJSONInteger :: Show JSONInteger

instance encodeJsonJSONInteger :: EncodeJson JSONInteger where
  encodeJson (JSONInteger x) = encodeJson $ toExponential $ intValue x

instance decodeJsonJSONInteger :: DecodeJson JSONInteger where
  decodeJson json = do
    s <- decodeJson json
    case parseBigNumber s of
      Left e -> fail (show e)
      Right x -> pure (JSONInteger x)

instance arbitraryJSONInteger :: Arbitrary JSONInteger where
  arbitrary = do
    n <- arbitrary
    s <- String.fromChars <$> arrayOf1 (elements $ NonEmpty '0' $ enumFromTo '1' '9')
    unsafePartial $ case parseBigNumber ((if n then "-" else "") <> s) of
      Right x -> pure (JSONInteger x)
    -- (x :: Number) <- arbitrary
    -- unsafePartial $ case Number.fromString $ trimDecimal $ show x of
    --   Just y -> pure (JSONInteger y)


trimDecimal :: String -> String
trimDecimal = String.fromChars <<< Array.takeWhile (\c -> c /= '.') <<< String.toChars
