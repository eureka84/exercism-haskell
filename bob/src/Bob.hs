module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor []  = "Fine. Be that way!"
responseFor xs
  | isSilence                = "Fine. Be that way!"
  | isShouted && isAQuestion = "Calm down, I know what I'm doing!"
  | isShouted                = "Whoa, chill out!"
  | isAQuestion              = "Sure."
  | otherwise                = "Whatever."
  where
    isShouted     = (not . null) onlyLetters && all isUpper onlyLetters
    onlyLetters   = filter isAlpha xs
    isAQuestion   = (== '?') $ last withoutSpaces
    isSilence     = null withoutSpaces
    withoutSpaces = filter (not . isSpace) xs

