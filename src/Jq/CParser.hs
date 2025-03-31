module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Parsing.Utils (stringAtom)

parseIdentity :: Parser Filter
parseIdentity = do
  _ <- token . char $ '.'
  return Identity

parseParentheses :: Parser Filter
parseParentheses = do
  _ <- token (char '(')
  f <- parseFilter
  _ <- token (char ')')
  return (Parentheses f)

parseStringIndexing :: Parser Filter
parseStringIndexing = 
    do 
    _ <- space
    _ <- char '.'
    _ <- symbol  "["
    _ <- char '\"'
    k <- (many stringAtom)
    _ <- char '\"'
    _ <- symbol  "]"
    return (StringIndexing k)
  <|> do
    _ <- space
    _ <- char '.'
    k <- anyIdent 
    return (StringIndexing k)



parseFilter :: Parser Filter
parseFilter =  parseStringIndexing <|> parseParentheses <|> parseIdentity

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
