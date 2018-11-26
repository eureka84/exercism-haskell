module Acronym (abbreviate) where

import Data.Char

splitCamelCase :: String -> [String]
splitCamelCase word = loop word [] []
  where loop [] curr res                  = res++[curr]
        loop (x:xs) curr res | isUpper x  = case curr of [] -> loop xs [x] res
                                                         s  -> loop xs [x] (res++[s])
        loop (x:xs) curr res              = loop xs (curr++[x]) res

split :: String -> [String]
split input = concat (splitCamelCase `map` (words input))

firstLetterCapitalized :: String -> Char
firstLetterCapitalized word = toUpper(head word)

abbreviate :: String -> String
abbreviate input = firstLetterCapitalized `map` (split input)
