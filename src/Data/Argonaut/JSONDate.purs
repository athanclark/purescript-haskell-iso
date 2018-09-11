module Data.Argonaut.JSONDate where

import Prelude
import Data.Int as Int
import Data.JSDate as JSDate
import Data.Date (Date)
import Data.Date as Date
import Data.DateTime.Locale (LocalValue (..))
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Enum (fromEnum)
import Data.String as String
import Data.Generic (class Generic, gEq)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.Argonaut as Argonaut
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser as Parser
import Text.Parsing.StringParser.String (regex)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Now (NOW, nowDate)
import Control.Monad.Eff.Exception (try)
import Test.QuickCheck (class Arbitrary)


newtype JSONDate = JSONDate Date

derive instance genericJSONDate :: Generic JSONDate

instance eqJSONDate :: Eq JSONDate where
  eq = gEq

getJSONDate :: JSONDate -> Date
getJSONDate (JSONDate x) = x

nowJSONDate :: forall eff. Eff (now :: NOW | eff) JSONDate
nowJSONDate = do
  LocalValue _ x <- nowDate
  pure (JSONDate x)

instance arbitraryJSONDate :: Arbitrary JSONDate where
  arbitrary = pure $ unsafePerformEff nowJSONDate

instance showJsonJSONDate :: Show JSONDate where
  show (JSONDate x) =
    let date' = JSDate.jsdate
          { year: Int.toNumber $ fromEnum $ Date.year x
          , month: Int.toNumber $ fromEnum (Date.month x) - 1
          , day: Int.toNumber $ fromEnum $ Date.day x
          , hour: 0.0
          , minute: 0.0
          , second: 0.0
          , millisecond: 0.0
          }
    in  String.take 10 $ unsafePerformEff $ JSDate.toISOString date'

instance encodeJsonJSONDate :: EncodeJson JSONDate where
  encodeJson = encodeJson <<< show

jsonDateParser :: Parser JSONDate
jsonDateParser = do
  s <- regex "\\d{4}-[01]\\d-[0-3]\\d"
  case unsafePerformEff $ try $ JSDate.parse s of
    Left _ -> Parser.fail "Not a date"
    Right x -> case JSDate.toDate x of
      Nothing -> Parser.fail "Not a date"
      Just y -> pure (JSONDate y)


instance decodeJsonJSONDate :: DecodeJson JSONDate where
  decodeJson json = do
    s <- decodeJson json
    case runParser jsonDateParser s of
      Left _ -> Argonaut.fail "Not a date"
      Right x -> pure x
