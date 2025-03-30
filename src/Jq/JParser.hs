module Jq.JParser where

import Parsing.Parsing
import Jq.Json
import Parsing.Utils (rawNumber, escapedString)

parseJNull :: Parser JSON
parseJNull = do _ <- string "null"
                return JNull
                -- alternative: JNull <$ string "null"


parseJBoolT :: Parser JSON
parseJBoolT = do    _ <- string "true"
                    return (JBool True)

parseJBoolF :: Parser JSON
parseJBoolF = do    _ <- string "false"
                    return (JBool True)
parseJBool :: Parser JSON
parseJBool = parseJBoolF <|> parseJBoolT

parseJString :: Parser JSON 
parseJString = fmap JString escapedString
{- parseJString = do 
                    _ <- char '"'
                    val <- (many alphanum)
                    _ <- char '"'
                    return (JString val)
                    -}

parseJNumber :: Parser JSON 
parseJNumber = do 
                    num <- rawNumber
                    return (JNumber (read num)) 


                
    
parseJSON :: Parser JSON
parseJSON = token (parseJNull <|> parseJBool <|> parseJString <|> parseJNumber)
