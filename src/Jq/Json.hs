module Jq.Json where
import Data.Char (toLower, ord, isControl)
import Data.List (intercalate)
import GHC.OldList (sortOn)
import Text.Printf

encodeUnicode :: Char -> String
encodeUnicode '\b' = "\\b"
encodeUnicode '\f' = "\\f"
encodeUnicode '\n' = "\\n"
encodeUnicode '\r' = "\\r"
encodeUnicode '\t' = "\\t"
encodeUnicode '"' = "\\\""
encodeUnicode '\\' = "\\\\"
encodeUnicode c
  | (ord c) >= 0x80 && (ord c) <= 0x9f = [c]
  | isControl (c) = printf "\\u%04x" (ord c)
  | otherwise = [c]

data JSON =
     JNull | JNumber Double | JString String | JBool Bool | JArray [JSON] | JObject [(String, JSON)]

mapInside :: [JSON] -> Int -> String 
mapInside js n =  intercalate (",\n" ++ concat (replicate n "  ")) (map show js)

instance Show JSON where
  show JNull   = "null"
  show (JNumber x)       = show x
  show (JString x) = '"' : (concatMap encodeUnicode x) ++  "\""
  show (JBool x) = map toLower (show x)
  show (JArray []) = "[]"
  show (JArray js) =  "[\n  " ++ (mapInside js 1)  ++ "\n]" -- fix nesting
  show (JObject entries) = "{" ++ intercalate ", " (map (\(x, y) -> (show x) ++ ": " ++ (show y)) entries) ++ "}"


instance Eq JSON where
  JNull == JNull = True
  JNumber a == JNumber b = a == b
  JString a == JString b = a == b
  JBool a == JBool b = a == b
  JArray a == JArray b = a == b
  JObject a == JObject b = sortOn fst a == sortOn fst b
  _ == _ = False


-- Smart constructors
-- These are included for test purposes and aren't meant to correspond one to one
-- with the actual constructors of the JSON datatype.
-- For the "weekly" tests to succeed fill them in so that they return
-- correct JSON values. Don't change the names or the signatures.

jsonNullSC :: JSON
jsonNullSC = JNull

jsonNumberSC :: Int -> JSON
jsonNumberSC x = JNumber (fromIntegral x)

jsonStringSC :: String -> JSON
jsonStringSC = JString

jsonBoolSC :: Bool -> JSON
jsonBoolSC = JBool

jsonArraySC :: [JSON] -> JSON
jsonArraySC = JArray

jsonObjectSC :: [(String, JSON)] -> JSON
jsonObjectSC = JObject
