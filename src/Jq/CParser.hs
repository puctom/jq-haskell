module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Parsing.Utils (stringAtom)
import Debug.Trace (trace)


{-
Precedence of operators:
Operator 	            Description
x? 	                  Error Suppression
-x 	                  Negative
*, /, % 	            Multiplication, Division, Modulo
+, - 	                Addition, Subtraction
==, !=, <, >,<=, >= 	Comparisons
and 	                Boolean AND
or 	                  Boolean OR
=, |=, +=, -=, *=, /=, %= 	Update-assignment
// 	                   Alternative
, 	                   Comma
| 	                    Pipe
label $variable 	Labels
try … catch … 	Try expression
if … then … end 	Conditional expression
foreach … as … (…) 	Loop expression
reduce … as … (…) 	Reduce expression
… as $variable 	Variable definition expression
def … ; … 	Function expression


-}


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
parseStringIndexing = parseOptionalStringIndexing <|> parseNonOptionalStringIndexing

parseNonOptionalStringIndexing :: Parser Filter
parseNonOptionalStringIndexing = parseGenStringIndex <|> parseSimpleStringIndex

parseOptionalStringIndexing :: Parser Filter 
parseOptionalStringIndexing = do 
    f <- parseNonOptionalStringIndexing
    _ <- char '?'
    return (OptStringIndexing f)

parseGenStringIndex :: Parser Filter 
parseGenStringIndex =
    do 
    _ <- space
    _ <- char '.'
    _ <- symbol  "["
    _ <- char '\"'
    k <- (many stringAtom)
    _ <- char '\"'
    _ <- symbol  "]"
    return (StringIndexing k)

parseSimpleStringIndex :: Parser Filter 
parseSimpleStringIndex = 
 do
    _ <- space
    _ <- char '.'
    k <- anyIdent 
    _ <- space
    return (StringIndexing k)

parsePipe :: Parser Filter
parsePipe = 
    do 
      f1 <- parseNonPipe -- level down
      _ <- token (char '|')
      f2 <- parseFilter -- all OR same level
      return (Pipe f1 f2)
    <|> 
    do 
      f1 <- parseStringIndexing
      _ <- space 
      f2 <- parseSimpleStringIndex
      return (Pipe f1 f2)

parseComma :: Parser Filter
parseComma = 
    do 
      f1 <- parsePipeLevel -- level down 
      _ <- token (char ',')
      f2 <- parseFilter -- all 
      return (Comma f1 f2)


parseFilter :: Parser Filter
parseFilter =  parseComma <|> parsePipeLevel

parsePipeLevel :: Parser Filter
parsePipeLevel =  parsePipe <|> parseNonPipe  

parseNonPipe :: Parser Filter 
parseNonPipe = parseStringIndexing <|> parseParentheses <|> parseIdentity

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right (trace ("Parsed as follows:\n " ++ show v) ConfigC v)
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
