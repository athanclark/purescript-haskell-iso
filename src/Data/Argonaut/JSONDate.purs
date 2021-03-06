module Data.Argonaut.JSONDate where

import Prelude
import Data.Int as Int
import Data.JSDate as JSDate
import Data.Date (Date)
import Data.Date as Date
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Enum (fromEnum)
import Data.String as String
import Data.Generic.Rep (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.Argonaut as Argonaut
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser as Parser
import Text.Parsing.StringParser.CodePoints (regex)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Now (nowDate)
import Effect.Exception (try)
import Test.QuickCheck (class Arbitrary)


newtype JSONDate = JSONDate Date

derive instance genericJSONDate :: Generic JSONDate _
derive newtype instance eqJSONDate :: Eq JSONDate

getJSONDate :: JSONDate -> Date
getJSONDate (JSONDate x) = x

nowJSONDate :: Effect JSONDate
nowJSONDate = JSONDate <$> nowDate

instance arbitraryJSONDate :: Arbitrary JSONDate where
  arbitrary = pure (unsafePerformEffect nowJSONDate)

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
    in  String.take 10 $ unsafePerformEffect $ JSDate.toISOString date'

instance encodeJsonJSONDate :: EncodeJson JSONDate where
  encodeJson = encodeJson <<< show

jsonDateParser :: Parser JSONDate
jsonDateParser = do
  s <- regex "\\d{4}-[01]\\d-[0-3]\\d"
  case unsafePerformEffect $ try $ JSDate.parse s of
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
