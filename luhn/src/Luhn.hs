module Luhn (isValid) where

import Data.Char
import Data.List.Index

isValid :: String -> Bool
isValid xs = isValid' filtered
  where filtered = filter (\x -> x /= ' ') xs

isValid' :: String -> Bool
isValid' [] = False
isValid' [_] = False
isValid' xs = (checkSum `mod` 10) == 0
  where
    checkSum = sum $ map (\(index, c) -> maybeDoubleBy index c) $ indexed $ reverse xsDigits
    xsDigits = map digitToInt xs
    maybeDoubleBy index digit
      | index `mod` 2 == 0 = digit
      | otherwise = if (double > 9) then double - 9 else double
          where double = 2 * digit