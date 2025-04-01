module Jq.JParser where

import Parsing.Parsing
import Jq.Json
import Jq.Utils (removeDuplicateKeys)
import Parsing.Utils (rawNumber, escapedString, stringAtom)

parseJNull :: Parser JSON
parseJNull = do _ <- string "null"
                return JNull
                -- alternative: JNull <$ string "null"


parseJBoolT :: Parser JSON
parseJBoolT = do    _ <- string "true"
                    return (JBool True)

parseJBoolF :: Parser JSON
parseJBoolF = do    _ <- string "false"
                    return (JBool False)
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

parseJArray :: Parser JSON 
parseJArray = do 
                    _ <- symbol "[" 
                    e1 <- parseJSON
                    elems <- many (do 
                                        _ <- symbol ","
                                        j <- parseJSON 
                                        return j     
                                )
                    _ <- symbol "]"
                    return (JArray (e1:elems))
            <|> 
                do 
                    _ <- symbol "[" 
                    _ <- symbol "]"
                    return (JArray [])

parseJObject :: Parser JSON 
parseJObject = do 
                    _ <- symbol "{"
                    _ <- space
                    _ <- char '\"' -- apparently can be without it 
                    k1 <- (many stringAtom)
                    _ <- symbol "\""
                    _ <- symbol ":"
                    v1 <- parseJSON 
                    elems <- many (do 
                                        _ <- symbol ","
                                        _ <- symbol "\""
                                        k <- many stringAtom
                                        _ <- symbol "\""
                                        _ <- symbol ":"
                                        v <- parseJSON 
                                        return (k,v)     
                                )
                    _ <- symbol "}"
                    return (JObject (removeDuplicateKeys((k1, v1) : elems)))
                <|> 
                    do 
                        _ <- symbol "{"
                        _ <- symbol "}"
                        return (JObject [])
                    
                


                
    
parseJSON :: Parser JSON
parseJSON = token (parseJNull <|> parseJBool <|> parseJString <|> parseJNumber <|> parseJArray <|> parseJObject)
