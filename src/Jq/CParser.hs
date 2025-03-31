module Jq.CParser where

import Parsing.Parsing
import Jq.Filters

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


parseFilter :: Parser Filter
parseFilter = parseIdentity <|> parseParentheses

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
