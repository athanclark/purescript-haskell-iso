module Data.Argonaut.JSONDateTime where

import Prelude
import Data.Int as Int
import Data.JSDate as JSDate
import Data.Time as Time
import Data.Date as Date
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.DateTime.Locale (LocalValue (..))
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Enum (fromEnum)
import Data.Generic (class Generic, gEq)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.Argonaut as Argonaut
import Data.String as String
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser as Parser
import Text.Parsing.StringParser.String (regex)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import Control.Monad.Eff.Exception (try)
import Test.QuickCheck (class Arbitrary)


newtype JSONDateTime = JSONDateTime DateTime

derive instance genericJSONDateTime :: Generic JSONDateTime

instance eqJSONDateTime :: Eq JSONDateTime where
  eq = gEq

getJSONDateTime :: JSONDateTime -> DateTime
getJSONDateTime (JSONDateTime x) = x

nowJSONDateTime :: forall eff. Eff (now :: NOW | eff) JSONDateTime
nowJSONDateTime = do
  LocalValue _ x <- nowDateTime
  pure (JSONDateTime x)

instance arbitraryJSONDate :: Arbitrary JSONDateTime where
  arbitrary = pure $ unsafePerformEff nowJSONDateTime

instance showJSONDateTime :: Show JSONDateTime where
  show (JSONDateTime x) =
    let date'' = DateTime.date x
        time'' = DateTime.time x
        date' = JSDate.jsdate
          { year: Int.toNumber $ fromEnum $ Date.year date''
          , month: Int.toNumber $ fromEnum (Date.month date'') - 1
          , day: Int.toNumber $ fromEnum $ Date.day date''
          , hour: Int.toNumber $ fromEnum $ Time.hour time''
          , minute: Int.toNumber $ fromEnum $ Time.minute time''
          , second: Int.toNumber $ fromEnum $ Time.second time''
          , millisecond: Int.toNumber $ fromEnum $ Time.millisecond time''
          }
        s = unsafePerformEff $ JSDate.toISOString date'
        y = case String.stripSuffix (String.Pattern "Z") s of
          Nothing -> s
          Just s' -> case String.stripSuffix (String.Pattern "0") s' of
            Nothing -> s' <> "Z"
            Just s'' -> case String.stripSuffix (String.Pattern "0") s'' of
              Nothing -> s'' <> "Z"
              Just s''' -> case String.stripSuffix (String.Pattern ".0") s''' of
                Nothing -> s''' <> "Z"
                Just s'''' -> s'''' <> "Z"
    in  y

instance encodeJsonJSONDateTime :: EncodeJson JSONDateTime where
  encodeJson = encodeJson <<< show

jsonDateTimeParser :: Parser JSONDateTime
jsonDateTimeParser = do
  s <- regex "\\d{4}-[01]\\d-[0-3]\\dT[0-2]\\d:[0-5]\\d:[0-5]\\d\\.\\d+([+-][0-2]\\d:[0-5]\\d|Z)"
  case unsafePerformEff $ try $ JSDate.parse s of
    Left _ -> Parser.fail "Not a datetime"
    Right x -> case JSDate.toDateTime x of
      Nothing -> Parser.fail "Not a datetime"
      Just y -> pure (JSONDateTime y)

instance decodeJsonJSONDateTime :: DecodeJson JSONDateTime where
  decodeJson json = do
    s <- decodeJson json
    case runParser jsonDateTimeParser s of
      Left _ -> Argonaut.fail "not a datetime"
      Right x -> pure x
