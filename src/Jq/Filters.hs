module Jq.Filters where

data Filter = Identity | StringIndexing String | Pipe Filter Filter | Comma Filter Filter

instance Show Filter where
  show (Identity) = "."
  show (StringIndexing s) = "." ++ s 
  show (Pipe f1 f2) = (show f1) ++ " | " ++ (show f2)
  show (Comma f1 f2) = (show f1) ++ ", " ++ (show f2)

instance Eq Filter where
  Identity == Identity = True
  (StringIndexing s1) == (StringIndexing s2) = s1 == s2 
  (Pipe f1 f2) == (Pipe f1' f2') = (f1 == f1') && (f2 == f2')
  (Comma f1 f2) == (Comma f1' f2') = (f1 == f1') && (f2 == f2')
  _ == _ = False


data Config = ConfigC {filters :: Filter}

-- Smart constructors
-- These are included for test purposes and
-- aren't meant to correspond one to one with actual constructors you add to Filter
-- For the tests to succeed fill them in with functions that return correct filters
-- Don't change the names or signatures, only the definitions

filterIdentitySC :: Filter
filterIdentitySC = Identity

filterStringIndexingSC :: String -> Filter
filterStringIndexingSC = StringIndexing

filterPipeSC :: Filter -> Filter -> Filter
filterPipeSC = Pipe

filterCommaSC :: Filter -> Filter -> Filter
filterCommaSC = Comma
