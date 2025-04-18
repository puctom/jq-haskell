module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Parsing.Utils (stringAtom)
import Debug.Trace (trace)
import Jq.JParser (parseJNull, parseJBool, parseJString, parseJNumber)

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
  return ((Parentheses f))

parseStringIndexing :: Parser Filter
parseStringIndexing = parseOptionalStringIndexing <|> parseNonOptionalStringIndexing

parseNonOptionalStringIndexing :: Parser Filter
parseNonOptionalStringIndexing = parseGenStringIndex <|> parseHalfGenStringIndex <|> parseSimpleStringIndex

parseOptionalStringIndexing :: Parser Filter
parseOptionalStringIndexing = do
    f <- parseNonOptionalStringIndexing
    _ <- symbol "?"
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
    _ <- symbol "?"
    return (OptStringIndexing f)


parseHalfGenStringIndex :: Parser Filter
parseHalfGenStringIndex =
  parseEscapedStringIndex
  <|>
    parseSimpleStringIndex

parseEscapedStringIndex :: Parser Filter 
parseEscapedStringIndex = 
    do
      _ <- space
      _ <- char '.'
      _ <- char '\"'
      k <- (many stringAtom)
      _ <- char '\"'
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
      f1 <- parseCommaLevel -- level down
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
      f1 <- parseSingleIndex -- level down 
      _ <- token (char ',')
      f2 <- parseCommaIndexing -- all 
      return (Comma f1 f2)
    <|> parseSingleIndex

parseSingleIndex :: Parser Filter 
parseSingleIndex = parseIndex <|> parseKey

parseComma :: Parser Filter -- TODO: This is right associative. Question to TA: how to make it left associative?
parseComma =
    do
      f1 <- parseAndLevel -- level down 
      _ <- token (char ',')
      f2 <- parseCommaLevel -- all 
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
parseKey :: Parser Filter
parseKey =
    do    
      _ <- char '\"'
      k <- (many stringAtom)
      _ <- char '\"'
      return (StringIndexing k)

parseTryCatch :: Parser Filter 
parseTryCatch = do
  _ <- symbol "try"
  f1 <- parseFilter
  _ <- symbol "catch"
  f2 <- parseFilter
  return (TryCatch f1 f2)

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
    _ <- symbol "?"
    return (OptSlice f)


parseNonOptIterator :: Parser Filter
parseNonOptIterator = parseNonEmptyIterator <|> parseEmptyIterator

parseOptIterator :: Parser Filter
parseOptIterator =
  do
    f <- parseNonEmptyIterator <|> parseEmptyIterator
    _ <- symbol "?"
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

parseObjEntry :: Parser (String, Filter)
parseObjEntry =
  do
    _ <- char '\"' -- apparently can be without it 
    k1 <- (many stringAtom)
    _ <- symbol "\""
    _ <- symbol ":"
    f1 <- parseFilter
    return (k1, f1)
    <|>
  do
    _ <- space
    k1 <- anyIdent
    _ <- symbol ":"
    f1 <- parseFilter
    return (k1, f1)


parseObjVal :: Parser Filter
parseObjVal = do
                    _ <- (symbol "{")
                    e1 <- parseObjEntry
                    elems <- many parseObjEntry
                    _ <- symbol "}"
                    return (ObjValConstr (e1 : elems))
            <|>
                do
                    _ <- symbol "{"
                    _ <- symbol "}"
                    return (ObjValConstr [])

parseRecDescent :: Parser Filter
parseRecDescent = do
                    _ <- symbol ".."
                    return (RecDescent)

parseNot :: Parser Filter 
parseNot = do 
  _ <- symbol "not"
  return Not

parseAnd :: Parser Filter 
parseAnd = do 
  f1 <- parseOrLevel
  _ <- symbol "and"
  f2 <- parseFilter
  return (And f1 f2)

parseOr :: Parser Filter
parseOr = do 
  f1 <- parseNonBoolOp
  _ <- symbol "or"
  f2 <- parseFilter
  return (Or f1 f2)

parseIfThenElse :: Parser Filter 
parseIfThenElse = 
    do 
      _ <-  symbol "if"
      f1 <- parseFilter 
      _ <- symbol "then"
      f2 <- parseFilter 
      _ <- symbol "else"
      f3 <- parseFilter 
      _ <- symbol "end"
      return (IfThenElse f1 f2 f3)

parseEqualityFilters :: Parser Filter 
parseEqualityFilters = do 
  f1 <- parseNonComparison 
  x <- operatorComp
  f2 <- parseAndLevel
  case x of 
      "==" -> return (Eq f1 f2)
      "!=" -> return (Neq f1 f2)
      "<" -> return (Smaller f1 f2)
      "<=" -> return (SmallerEq f1 f2)
      ">" -> return (Greater f1 f2)
      ">=" -> return (GreaterEq f1 f2)
      _ -> empty 
  
parseArithmeticOperators :: Parser Filter 
parseArithmeticOperators = 
  do 
    f1 <- parseNonArithm 
    x <- operatorOp
    f2 <- parseFilter
    case x of 
      "+" -> return (Add f1 f2)
      "-" -> return (Subtract f1 f2)
      "*" -> return (Multiply f1 f2)
      "/" -> return (Divide f1 f2)
      _ -> empty 



-- parseCompOperators :: Parser Filter 
-- parseCompOperators = parseSmaller <|> parseSmallerEq <|> parseGreater <|> parseGreaterEq 

-- parseSmaller :: Parser Filter
-- parseSmaller = do 
--   f1 <- parseNonComparison
--   _ <- symbol "<"
--   f2 <- parseFilter 
--   return (Smaller f1 f2)

-- parseSmallerEq :: Parser Filter
-- parseSmallerEq = do 
--   f1 <- parseNonComparison
--   _ <- symbol "<="
--   f2 <- parseFilter 
--   return (SmallerEq f1 f2)

-- parseGreater :: Parser Filter
-- parseGreater = do 
--   f1 <- parseNonComparison
--   _ <- symbol ">"
--   f2 <- parseFilter 
--   return (Greater f1 f2)

-- parseGreaterEq :: Parser Filter
-- parseGreaterEq = do 
--   f1 <- parseNonComparison
--   _ <- symbol ">="
--   f2 <- parseFilter 
--   return (GreaterEq f1 f2)



parseValConstr :: Parser Filter
parseValConstr = parseSimpleVal <|> parseArrayVal <|> parseObjVal

parseIterator :: Parser Filter
parseIterator = parseOptIterator <|> parseNonOptIterator -- TODO: refactor to reduce copy paste, abstract the optional ones

parseFilter :: Parser Filter
parseFilter = parseEqualityFilters <|> parseNonComparison

parseNonComparison :: Parser Filter
parseNonComparison = parseArithmeticOperators <|>  parseNonArithm 

parseNonArithm :: Parser Filter
parseNonArithm = parseTryCatch <|> parseIfThenElse <|> parseRegFilter

parseRegFilter :: Parser Filter
parseRegFilter = parsePipe <|> parseCommaLevel

parseCommaLevel :: Parser Filter
parseCommaLevel =  parseComma <|> parseAndLevel

parseAndLevel :: Parser Filter
parseAndLevel = parseAnd <|> parseOrLevel

parseOrLevel :: Parser Filter
parseOrLevel = parseOr <|> parseNonBoolOp

parseNonBoolOp :: Parser Filter
parseNonBoolOp = parseNot <|> parseSlice <|> parseIterator <|> parseStringIndexing  <|> parseParentheses <|> parseValConstr <|> parseRecDescent <|> parseIdentity -- <|> parseValConstr

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _  ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right (trace ("Parsed as follows:\n " ++ show v) ConfigC v)
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
