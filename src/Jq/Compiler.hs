module Jq.Compiler where

import           Jq.Filters
import           Jq.Json
import Debug.Trace (trace)

type JProgram a = JSON -> Either String a

compileTrace :: Filter -> JProgram [JSON]
compileTrace f inp = trace ("Called compile with " ++ show f ++ " and: " ++ show inp) (compile f inp)

compile :: Filter -> JProgram [JSON]
-- compile (ValConstr v) inp = return [v]
compile (SimpleValConstr j) _ = return [j]
compile (ObjValConstr []) _ = return [JObject []]
compile (ObjValConstr arr) inp = case entries of
                                        Left x -> Left x
                                        Right y -> return [JObject y]
                                    where entries = traverse (\(key, filt) -> case compile filt inp of
                                                                Left x -> Left x
                                                                Right y -> Right (key, head y) ) arr
compile (ArrValConst Nothing) _ = return ([JArray []])
compile (ArrValConst (Just a)) inp = (compile a inp) >>= (\resultJsonArray -> return [JArray (resultJsonArray)] )
compile Identity inp = return [inp]
compile (Slice k1 k2) (JString s) = return [JString (drop k1' (take k2' s))] where
        k1' = case k1 of
            Nothing -> 0
            (Just x) -> if x < 0 then x + (length s) else x
        k2' = case k2 of
            Nothing -> (length s)
            (Just x) -> if x < 0 then x + (length s) else x
compile (Slice k1 k2) (JArray arr) = return [JArray (drop k1' (take k2' arr))] where -- TODO: horrible code duplication
        k1' = case k1 of
            Nothing -> 0
            (Just x) -> if x < 0 then x + (length arr) else x
        k2' = case k2 of
            Nothing -> (length arr)
            (Just x) -> if x < 0 then x + (length arr) else x
compile (Slice _ _) JNull = return [JNull]
compile (Index k) (JArray arr)
    | k >= length arr = return [JNull]
    | k >= 0 = return [arr !! k ] -- just element at position k
    | k >= -(length arr) = return [arr !! (k + length arr) ]
    | otherwise = return [JNull] -- too much negative
compile (Index _) JNull = return [JNull]
compile (Index _) _ = Left "Cannot apply index to non-arary"
compile (OptSlice f) (JArray arr) = compileTrace f (JArray arr)
compile (OptSlice f) (JString arr) = compileTrace f (JString arr)
compile (OptSlice _) JNull = return [JNull]
compile (OptSlice _) _ = return []
compile (OptIterator f) (JArray arr) = compileTrace f (JArray arr)
compile (OptIterator (Iterator Nothing)) (JObject obj) = compileTrace (Iterator Nothing) (JObject obj)
compile (OptIterator (Iterator (Just _))) (JNull) = return [JNull]
compile (OptIterator _) _ = return []
compile (Iterator (Just f)) (JNull) = compileTrace f (JNull)
compile (Iterator Nothing) (JArray arr) = return arr
compile (Iterator (Just f)) (JArray arr) = compileTrace f (JArray arr)
compile (Iterator Nothing) (JObject obj) = return (map snd obj)
compile (OptStringIndexing s) (JObject a) = compileTrace s (JObject a) -- TODO: would be better not to recreate JObject
compile (OptStringIndexing _) JNull = return [JNull]
compile (OptStringIndexing _) _ = return []
compile (StringIndexing s) (JObject a) = case map snd (filter (\(key, val) -> key == s) a) of
                                            [] -> return [JNull]
                                            xs -> return xs
compile (StringIndexing _) JNull = return [JNull]
compile (StringIndexing _) x = Left ("Cannot string-index " ++ show x)
compile (Pipe f1 f2) inp = case (compileTrace f1 inp) of
                            Right arr -> foldl (\acc val ->
                                --          Either String [JSON], JSON
                                -- Applies f2 to each elem of arr where arr is output of compile f1 inp
                                do
                                    acc2 <- acc
                                    e2 <- compileTrace f2 val
                                    return (acc2 ++ e2)

                                -- case acc of
                                --                                 Left x -> Left x
                                --                                 Right succArr ->  case (compile f2 val) of
                                --                                     Left x -> Left x
                                --                                     Right jsonListRes -> Right (succArr ++ jsonListRes)
                                                                ) (Right []) arr
                            Left x -> Left x
compile (Comma f1 f2) inp = do
                                e1 <- compileTrace f1 inp
                                e2 <- compileTrace f2 inp
                                return (e1 ++ e2)
                            -- compile f1 inp >>= (\e1 ->
                            -- compile f2 inp >>= (\e2 ->
                            --     return (e1 ++ e2)))
compile (Parentheses f) inp = compileTrace f inp
compile _ _ = Left "Incorrect invocation"

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
