module Jq.Compiler where

import           Jq.Filters
import           Jq.Json


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile Identity inp = return [inp]
compile (StringIndexing s) (JObject a) = case map snd (filter (\(key, val) -> key == s) a) of
                                            [] -> return [JNull]
                                            xs -> return xs
compile (StringIndexing s) JNull = return [JNull]

compile (Pipe f1 f2) inp = case (compile f1 inp) of
                            Right arr -> foldl (\acc val ->
                                --          Either String [JSON], JSON
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

compile _ _ = Left "Incorrect invocation"

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
