module LeapYear (isLeapYear) where

isDivisibleBy :: Integer -> Integer -> Bool
isDivisibleBy number divisor = (number `mod` divisor) == 0

isLeapYear :: Integer -> Bool
isLeapYear year = (year `isDivisibleBy` 4 && not(year `isDivisibleBy` 100))
                || year `isDivisibleBy` 400
