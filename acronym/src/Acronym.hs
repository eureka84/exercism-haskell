module Acronym (abbreviate) where

import Data.Char
import Data.List.Split

splitCamelCase :: String -> [String]
splitCamelCase word = loop word [] []
  where loop remaining curr result = case remaining of
      []                           -> result++[curr]
      (first:rest) | isUpper first -> case curr of
                                        []                    -> loop rest [first] result
                                        s | isLower (last s)  -> loop rest [first] (result++[s])
                                        _                     -> loop rest (curr++[first]) result
      (first:rest)                 -> loop rest (curr++[first]) result

separateWords :: String -> [String]
separateWords phrase = filter (not . null) (splitOneOf " -," phrase)

splitInput :: String -> [String]
splitInput input = concat $ map splitCamelCase $ separateWords input

abbreviate :: String -> String
abbreviate input = map (toUpper . head) (splitInput input)
