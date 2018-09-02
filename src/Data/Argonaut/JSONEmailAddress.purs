module Data.Argonaut.JSONEmailAddress where

import Text.Email.Validate (EmailAddress)
import Text.Email.Validate as Email

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (Right))
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Yarn as String
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail)
import Data.Typelevel.Undefined (undefined)
import Data.NonEmpty (NonEmpty (..))
import Data.Enum (enumFromTo)
import Data.List.Lazy (replicateM)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Console (log)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (elements, sized, resize, chooseInt)
import Partial.Unsafe (unsafePartial)


-- FIXME restrict to 64 x 63 chars

newtype JSONEmailAddress = JSONEmailAddress EmailAddress

derive instance genericJSONEmailAddress :: Generic JSONEmailAddress
derive newtype instance eqJSONEmailAddress :: Eq JSONEmailAddress
-- derive newtype instance encodeJsonJSONEmailAddress :: EncodeJson JSONEmailAddress
-- derive newtype instance decodeJsonJSONEmailAddress :: DecodeJson JSONEmailAddress

instance encodeJsonJSONEmailAddress :: EncodeJson JSONEmailAddress where
  encodeJson (JSONEmailAddress x) = encodeJson (Email.toString x)

instance decodeJsonJSONEmailAddress :: DecodeJson JSONEmailAddress where
  decodeJson json = do
    s <- decodeJson json
    case Email.emailAddress s of
      Nothing -> fail "JSONEmailAddress"
      Just e -> pure (JSONEmailAddress e)

instance showJSONEmailAddress :: Show JSONEmailAddress where
  show (JSONEmailAddress x) = Email.toString x

instance arbitraryJSONEmailAddress :: Arbitrary JSONEmailAddress where
  arbitrary = do
    -- let isChar c =
    --       let r = unsafePartial $ case regex "\\s|\\c" noFlags of
    --                 Right x -> x
    --       in  not $ test r $ String.fromChars [c]
    name <- arbitraryNonEmptyAscii 64
    domain <- arbitraryNonEmptyAscii 63
    let x = name <> "@" <> domain <> ".com"
    unsafePartial $ case Email.emailAddress x of
      Just e -> pure (JSONEmailAddress e)
      Nothing -> unsafePerformEff $ undefined <$ log x
    where
      arbitraryNonEmptyAscii maxS = do
        l <- chooseInt 1 maxS
        String.fromChars <$> replicateM l (elements $ NonEmpty 'a' $ enumFromTo 'b' 'z')


scale f x = sized \i -> resize (f i) x
