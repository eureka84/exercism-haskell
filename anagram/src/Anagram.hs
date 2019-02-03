module Anagram (anagramsFor) where

import Data.List
import Data.Char

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter (\x -> x `isAnagramOf` xs) xss

isAnagramOf :: String -> String -> Bool
isAnagramOf candidate original = hasSameLetters && not(sameWord)
  where
    lOriginal = map toLower original
    lCandidate = map toLower candidate
    hasSameLetters = (sort lCandidate) == (sort lOriginal)
    sameWord = lCandidate == lOriginal

