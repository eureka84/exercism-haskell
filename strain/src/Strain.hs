module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard p xs = case xs of [] -> []
                          (y:ys) -> if p y then discard p ys else y:(discard p ys)

keep :: (a -> Bool) -> [a] -> [a]
keep p xs = case xs of [] -> []
                       (y:ys) -> if p y then y:(keep p ys) else keep p ys
