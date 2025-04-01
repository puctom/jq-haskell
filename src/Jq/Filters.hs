module Jq.Filters where

data Filter = Identity 
        | StringIndexing String 
        | Pipe Filter Filter 
        | Comma Filter Filter 
        | Parentheses Filter
        | OptStringIndexing Filter

instance Show Filter where
  show (Identity) = "  " ++ "FId" ++ "  "
  show (StringIndexing s) = "  " ++ "FSIdxS(/" ++ s ++ "/FSIdxE)" ++ "  "
  show (Pipe f1 f2) = "  " ++ "FPipeS(" ++ (show f1) ++ " | " ++ (show f2) ++ "FPipeE)" ++ "  "
  show (Comma f1 f2) = "  " ++ "FCommaS(" ++ (show f1) ++ ", " ++ (show f2) ++ "FCommaE)" ++ "  "
  show (Parentheses f) = "  " ++ "FParS(" ++ (show f) ++ "FParE)"  ++ "  "
  show (OptStringIndexing f) = "  " ++ "FSIdx?S(" ++ (show f) ++ "FSIdx?E)"  ++ "  "

instance Eq Filter where
  Identity == Identity = True
  (StringIndexing s1) == (StringIndexing s2) = s1 == s2 
  (Pipe f1 f2) == (Pipe f1' f2') = (f1 == f1') && (f2 == f2')
  (Comma f1 f2) == (Comma f1' f2') = (f1 == f1') && (f2 == f2')
  (OptStringIndexing f) == (OptStringIndexing f')  = f == f'
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
