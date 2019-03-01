module Pangram (isPangram) where

import Data.Char (isAlpha, toLower)

isPangram :: String -> Bool
isPangram text = all (`elem` letters) ['a' .. 'z']
    where letters = map toLower $ filter isAlpha text
