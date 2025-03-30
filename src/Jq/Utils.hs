module Jq.Utils where
import Jq.Json

removeDuplicateKeys :: [(String, JSON)] -> [(String, JSON)]
removeDuplicateKeys entries = foldr
                        (\ (key, val) acc ->
                            if key `elem` map fst acc
                            then
                                filter (\(x,_) -> x == key) acc ++ (filter (\(x,_) -> x /= key) acc)
                            else 
                                (key, val) : acc
                        ) [] entries