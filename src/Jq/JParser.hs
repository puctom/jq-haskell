module Jq.JParser where

import Parsing.Parsing
import Jq.Json

parseJNull :: Parser JSON
parseJNull = do _ <- string "null"
                return JNull


parseJBoolT :: Parser JSON
parseJBoolT = do    _ <- string "true"
                    return (JBool True)

parseJBoolF :: Parser JSON
parseJBoolF = do    _ <- string "false"
                    return (JBool True)
parseJBool :: Parser JSON
parseJBool = parseJBoolF <|> parseJBoolT

parseJString :: Parser JSON 
parseJString = do 
                    _ <- char '"'
                    val <- (many alphanum)
                    _ <- char '"'
                    return (JString val)

parseJNumber :: Parser JSON 
parseJNumber = do 
                    a <- double 
                    return (JNumber a)


                
    
parseJSON :: Parser JSON
parseJSON = token (parseJNull <|> parseJBool <|> parseJString <|> parseJNumber)
