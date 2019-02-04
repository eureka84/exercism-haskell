module Prime (nth) where

nth :: Int -> Maybe Integer
nth 0 = Nothing
nth n = Just $ primes !! (n-1)

primes :: [Integer]
primes = filter isPrime [2..]

isPrime :: Integer -> Bool
isPrime 2 = True
isPrime 3 = True
isPrime n = not $ any dividesN [2..intSqrtN]
  where dividesN x = n `mod` x == 0
        intSqrtN = truncate sqrtN
        sqrtN :: Double
        sqrtN = sqrt $ fromIntegral n