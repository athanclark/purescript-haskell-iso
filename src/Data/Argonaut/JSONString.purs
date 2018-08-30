module Data.Argonaut.JSONString
  (JSONString, jsonString, getJSONString) where

import Prelude

import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Tuple (Tuple (..))
import Data.These (These (..))
import Data.Array as Array
import Data.String.Yarn as String
import Data.String.Normalize (nfkc)
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.String (anyChar, string)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Control.Alternative ((<|>))
import Control.Monad.Rec.Class (tailRecM, Step (..))
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, arbitrary)


newtype JSONString = JSONString String

derive instance genericJSONString :: Generic JSONString
derive newtype instance showJSONString :: Show JSONString
-- derive newtype instance eqJSONString :: Eq JSONString
derive newtype instance ordJSONString :: Ord JSONString
derive newtype instance encodeJsonJSONString :: EncodeJson JSONString
derive newtype instance decodeJsonJSONString :: DecodeJson JSONString

instance arbitraryJSONString :: Arbitrary JSONString where
  arbitrary = jsonString <$> arbitrary

instance eqJSONString :: Eq JSONString where
  eq (JSONString xs) (JSONString ys) = eq ((String.toChars xs) :: Array _) (String.toChars ys)


jsonString :: String -> JSONString
jsonString = JSONString -- <<< nfkc -- <<< show


getJSONString :: JSONString -> String
getJSONString (JSONString x) = x -- unsafePartial $ case runParser unescape x of
  -- Right y -> y
  -- where
  --   unescape = tailRecM go ""
  --     where
  --       go :: String -> Parser (Step String String)
  --       go acc = do
  --         let backslash = '\\' <$ string "\\\\"
  --             backspace = '\b' <$ string "\\b"
  --             feed = '\f' <$ string "\\f"
  --             newline = '\n' <$ string "\\n"
  --             return = '\r' <$ string "\\r"
  --             tab = '\t' <$ string "\\t"
  --             quote = '"' <$ string "\\\""

  --             escaped = backslash <|> backspace <|> feed <|> newline <|> return
  --               <|> tab <|> quote

  --         mC <- (Just <$> escaped) <|> optionMaybe anyChar
  --         case mC of
  --           Nothing -> pure (Done acc)
  --           Just c -> pure $ Loop $ String.snoc acc c

-- diffJSONString :: JSONString -> JSONString -> Maybe (These JSONString JSONString)
-- diffJSONString (JSONString a) (JSONString b) =
--   case diffArray (String.toChars a) (String.toChars b) of
--     Nothing -> Nothing
--     Just d -> Just $ case d of
--       This as -> This (fromChars as)
--       That bs -> That (fromChars bs)
--       Both as bs -> Both (fromChars as) (fromChars bs)
--   where
--     fromChars :: Array Char -> JSONString
--     fromChars = JSONString <<< String.fromChars

--     diffArray :: forall a. Eq a => Array a -> Array a -> Maybe (These (Array a) (Array a))
--     diffArray as bs = case Tuple (Array.uncons as) (Array.uncons bs) of
--       Tuple (Just {head:ah,tail:at}) (Just {head:bh,tail:bt})
--         | ah == bh -> diffArray at bt
--         | otherwise -> Just (Both as bs)
--       Tuple Nothing (Just _) -> Just (That bs)
--       Tuple (Just _) Nothing -> Just (This as)
--       Tuple Nothing Nothing -> Nothing
