module Pangram (isPangram) where

import qualified Data.Set as Set
import Data.Char
import Data.List

-- working for infinite strings
isPangram :: String -> Bool
isPangram = any ifPangram  . map toCharactersSet . inits
  where
    ifPangram xs = alphabet == xs
    alphabet = Set.fromList ['a'..'z']
    toCharactersSet = Set.fromList . map toLower . filter isAlpha
