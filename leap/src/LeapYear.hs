module LeapYear (isLeapYear) where

isDivisibleBy :: Integer -> Integer -> Bool
isDivisibleBy number divisor = (number `mod` divisor) == 0

isDivisibleBy400 :: Integer -> Bool
isDivisibleBy400 number = number `isDivisibleBy` 400

isDivisibleBy100 :: Integer -> Bool
isDivisibleBy100 number = number `isDivisibleBy` 100

isDivisibleBy4 :: Integer -> Bool
isDivisibleBy4 number = number `isDivisibleBy` 4

isLeapYear :: Integer -> Bool
isLeapYear year = (isDivisibleBy400 year)
               || (isDivisibleBy4 year && not(isDivisibleBy100 year))
