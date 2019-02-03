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
separateWords phrase = filter (not . null) (splitOneOf " -," phrase)

splitInput :: String -> [String]
splitInput input = concat $ map splitCamelCase $ separateWords input

abbreviate :: String -> String
abbreviate input = map (toUpper . head) (splitInput input)
