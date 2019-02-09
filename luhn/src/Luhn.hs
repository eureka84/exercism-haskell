module Luhn (isValid) where

import Data.Char
import Data.List.Index

isValid :: String -> Bool
isValid xs = isValid' withoutSpaces
             where
               withoutSpaces = filter (\x -> x /= ' ') xs

isValid' :: String -> Bool
isValid' []  = False
isValid' [_] = False
isValid' xs  = (checkSum `mod` 10) == 0
  where
    checkSum = sum $ imap doubleEverySecondDigit $ reverse xsDigits
    xsDigits = map digitToInt xs
    doubleEverySecondDigit index digit
      | index `mod` 2 == 0 = digit
      | otherwise          = double digit

double :: Int -> Int
double n
  | nDoubled > 9  = nDoubled - 9
  | otherwise     = nDoubled
  where
    nDoubled = 2 * n


