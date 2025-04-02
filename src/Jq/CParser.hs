module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Parsing.Utils (stringAtom)
import Debug.Trace (trace)
import Jq.JParser (parseJNull, parseJBool, parseJString, parseJNumber)
import Jq.Json (JSON)


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
parseNonOptionalStringIndexing = parseGenStringIndex <|> parseHalfGenStringIndex <|> parseSimpleStringIndex

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
    _ <- symbol "["
    _ <- char '\"'
    k <- (many stringAtom)
    _ <- char '\"'
    _ <- symbol  "]"
    return (StringIndexing k)

parseOptHalfGenStringIndex :: Parser Filter 
parseOptHalfGenStringIndex = 
  do 
    f <- parseHalfGenStringIndex
    _ <- char '?'
    return (OptStringIndexing f)


parseHalfGenStringIndex :: Parser Filter 
parseHalfGenStringIndex =
    do 
    _ <- space
    _ <- char '.'
    _ <- char '\"'
    k <- (many stringAtom)
    _ <- char '\"'
    return (StringIndexing k)
  <|> 
    parseSimpleStringIndex


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
    <|> parseStringPipe


parseStringPipe :: Parser Filter 
parseStringPipe =
    do 
      f1 <- parseStringIndexing
      _ <- space 
      f2 <- parseStringPipe
      return (Pipe f1 f2)
    <|> 
    do
      f1 <- parseStringIndexing
      _ <- space 
      f2 <- parseOptHalfGenStringIndex <|> parseHalfGenStringIndex
      return (Pipe f1 f2)

parseCommaIndexing :: Parser Filter -- TODO: This is right associative. Question to TA: how to make it left associative?
parseCommaIndexing = 
    do 
      f1 <- parseIndex -- level down 
      _ <- token (char ',')
      f2 <- parseCommaIndexing -- all 
      return (Comma f1 f2)
    <|> parseIndex

parseComma :: Parser Filter -- TODO: This is right associative. Question to TA: how to make it left associative?
parseComma = 
    do 
      f1 <- parsePipeLevel -- level down 
      _ <- token (char ',')
      f2 <- parseFilter -- all 
      return (Comma f1 f2)

parseNonEmptyIterator :: Parser Filter 
parseNonEmptyIterator = 
    do 
      _ <- symbol "."
      _ <- symbol "["
      f <- parseCommaIndexing -- parseFilter <|> parseIndex -- TODO: here should more generic
      _ <- symbol "]"
      return (Iterator (Just f))
   

parseEmptyIterator :: Parser Filter 
parseEmptyIterator = 
    do 
      _ <- symbol "."
      _ <- symbol "["
      _ <- space
      _ <- symbol "]"
      return (Iterator Nothing)

parseIndex :: Parser Filter 
parseIndex = 
    do 
      k <- integer
      return (Index k)

parseSlice :: Parser Filter
parseSlice = parseOptSlice <|> parseNonOptSlice

parseNonOptSlice :: Parser Filter 
parseNonOptSlice = parseSliceTwoInt <|> parseSliceFirstInt <|> parseSliceSecondInt

parseSliceTwoInt :: Parser Filter 
parseSliceTwoInt = 
    do 
      _ <- symbol "."
      _ <- symbol "["
      f1 <- integer -- can it be all filters???
      _ <- symbol ":"
      f2 <- integer
      _ <- symbol "]"
      return (Slice (Just f1) (Just f2))

parseSliceFirstInt :: Parser Filter 
parseSliceFirstInt = 
    do 
      _ <- symbol "."
      _ <- symbol "["
      f1 <- integer -- TODO: add optional handling
      _ <- symbol ":"
      _ <- symbol "]"
      return (Slice (Just f1) Nothing)

parseSliceSecondInt :: Parser Filter 
parseSliceSecondInt = 
    do 
      _ <- symbol "."
      _ <- symbol "["
      _ <- symbol ":"
      f2 <- integer -- TODO: add optional handling
      _ <- symbol "]"
      return (Slice Nothing (Just f2))

parseOptSlice :: Parser Filter 
parseOptSlice = 
  do
    f <- parseNonOptSlice
    _ <- char '?'
    return (OptSlice f)


parseNonOptIterator :: Parser Filter
parseNonOptIterator = parseNonEmptyIterator <|> parseEmptyIterator

parseOptIterator :: Parser Filter
parseOptIterator = 
  do
    f <- parseNonEmptyIterator <|> parseEmptyIterator
    _ <- char '?'
    return (OptIterator f)

parseSimpleVal :: Parser Filter 
parseSimpleVal = 
  do
    j <- parseJNull <|> parseJBool <|> parseJString <|> parseJNumber
    return (SimpleValConstr j)

parseArrayVal :: Parser Filter 
parseArrayVal = do 
                    _ <- symbol "[" 
                    e1 <- parseFilter
                    _ <- symbol "]"
                    return (ArrValConst (Just e1))
            <|> 
                do 
                    _ <- symbol "[" 
                    _ <- symbol "]"
                    return (ArrValConst Nothing)


parseValConstr :: Parser Filter 
parseValConstr = parseSimpleVal <|> parseArrayVal

parseIterator :: Parser Filter
parseIterator = parseOptIterator <|> parseNonOptIterator -- TODO: refactor to reduce copy paste, abstract the optional ones

parseFilter :: Parser Filter
parseFilter =  parseComma <|> parsePipeLevel

parsePipeLevel :: Parser Filter
parsePipeLevel =  parsePipe <|> parseNonPipe  

parseNonPipe :: Parser Filter 
parseNonPipe = parseSlice <|> parseIterator <|> parseStringIndexing <|> parseParentheses <|>  parseValConstr <|> parseIdentity -- <|> parseValConstr

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _  ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right (trace ("Parsed as follows:\n " ++ show v) ConfigC v)
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
