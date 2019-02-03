module Acronym (abbreviate) where

import Data.Char
import Data.List.Split

splitCamelCase :: String -> [String]
splitCamelCase word = loop word [] []
  where loop remaining curr res = case remaining of
          []                  -> res++[curr]
          (x:xs) | isUpper x  -> case curr of
                                    []                    -> loop xs [x] res
                                    s | isLower (last s)  -> loop xs [x] (res++[s])
                                    _                     -> loop xs (curr++[x]) res
          (x:xs)              -> loop xs (curr++[x]) res

separateWords :: String -> [String]
separateWords = filter (not . null) .  splitOneOf " -,"

splitInput :: String -> [String]
splitInput = concat . map splitCamelCase . separateWords

abbreviate :: String -> String
abbreviate = map (toUpper . head) . splitInput
