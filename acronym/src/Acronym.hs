module Acronym (abbreviate) where

import Data.Char

abbreviate :: String -> String
abbreviate xs =
   (\x -> toUpper(head x)) `map` (words xs)
