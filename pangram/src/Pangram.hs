module Pangram (isPangram) where

import qualified Data.Set as Set
import Data.Char

isPangram :: String -> Bool
isPangram text = textCharactersSet == alphabet
  where
    textCharactersSet = Set.fromList $ map toLower $ filter isAlpha text
    alphabet = Set.fromList ['a'..'z']