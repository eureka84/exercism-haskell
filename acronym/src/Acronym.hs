module Acronym (abbreviate) where

import Data.Char
import Data.List
import Data.List.Split

splitCamelCase :: String -> [String]
splitCamelCase word
 | all isLower word = [word]
 | all isUpper word = [word]
 | otherwise        = map (\(f, s) -> f ++ s) $
                      filter (\(f,_) -> length f == 1) $
                      zip (take (length grouped - 1) grouped) (drop 1 grouped)
  where
    grouped = groupBy (\x y -> isLower x && isLower y) $ filter isAlpha word

separateWords :: String -> [String]
separateWords = filter (not . null) .  splitOneOf " -,"

splitInput :: String -> [String]
splitInput = concat . map splitCamelCase . separateWords

abbreviate :: String -> String
abbreviate = map (toUpper . head) . splitInput
