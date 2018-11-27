module Acronym (abbreviate) where

import Data.Char
import Data.List.Split

splitCamelCase :: String -> [String]
splitCamelCase word = loop word [] []
  where loop [] curr res                = res++[curr]
        loop (x:xs) [] res | isUpper x  = loop xs [x] res
        loop (x:xs) curr res | isUpper x && isLower (last curr)
                                        = loop xs [x] (res++[curr])
        loop (x:xs) curr res            = loop xs (curr++[x]) res

separateWords :: String -> [String]
separateWords phrase = filter (\x ->  not(null x)) (splitOneOf " -," phrase)

splitInput :: String -> [String]
splitInput input = concat (map splitCamelCase (separateWords input))

abbreviate :: String -> String
abbreviate input = map (toUpper . head) (splitInput input)
