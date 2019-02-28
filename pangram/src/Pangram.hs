module Pangram (isPangram) where

import qualified Data.Set as Set
import Data.Char
import Data.List

-- working for infinite strings
isPangram :: String -> Bool
isPangram = (not . null) . filter ifPangram  . map toCharactersSet . inits
  where
    ifPangram xs = (Set.isSubsetOf alphabet  xs) && (Set.isSubsetOf xs alphabet)
    alphabet = Set.fromList ['a'..'z']
    toCharactersSet = Set.fromList . map toLower . filter isAlpha
