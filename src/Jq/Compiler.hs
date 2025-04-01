module Jq.Compiler where

import           Jq.Filters
import           Jq.Json


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile Identity inp = return [inp]
compile (OptStringIndexing s) (JObject a) = compile s (JObject a) -- TODO: would be better not to recreate JObject
compile (OptStringIndexing _) _ = return [] -- TODO: would be better not to recreate JObject
compile (StringIndexing s) (JObject a) = case map snd (filter (\(key, val) -> key == s) a) of
                                            [] -> return [JNull]
                                            xs -> return xs
compile (StringIndexing _) JNull = return [JNull]
compile (StringIndexing _) x = Left ("Cannot string-index " ++ show x)
compile (Pipe f1 f2) inp = case (compile f1 inp) of
                            Right arr -> foldl (\acc val ->
                                --          Either String [JSON], JSON
                                -- Applies f2 to each elem of arr where arr is output of compile f1 inp
                                do
                                    acc2 <- acc
                                    e2 <- compile f2 val
                                    return (acc2 ++ e2)

                                -- case acc of
                                --                                 Left x -> Left x
                                --                                 Right succArr ->  case (compile f2 val) of
                                --                                     Left x -> Left x
                                --                                     Right jsonListRes -> Right (succArr ++ jsonListRes)
                                                                ) (Right []) arr
                            Left x -> Left x
compile (Comma f1 f2) inp = do
                                e1 <- compile f1 inp
                                e2 <- compile f2 inp
                                return (e1 ++ e2)
                            -- compile f1 inp >>= (\e1 ->
                            -- compile f2 inp >>= (\e2 ->
                            --     return (e1 ++ e2)))
compile (Parentheses f) inp = compile f inp 
compile _ _ = Left "Incorrect invocation"

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
