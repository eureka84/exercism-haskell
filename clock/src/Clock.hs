module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock {
   hour :: Int,
   minutes :: Int
} deriving (Eq)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min
  | (abs hour) > 24 = fromHourMin ((signum hour) * ((abs hour) `mod` 24)) min
  | hour < 0        = fromHourMin (24 + hour) min
  | min < 0         = fromHourMin (hour - 1) (60 + min)
  | min >= 60       = fromHourMin (hour + min `div` 60) (min `mod` 60)
  | otherwise       = Clock (hour `mod` 24) min

toString :: Clock -> String
toString (Clock h m) = (format h) ++ ":" ++ (format m)

format :: Int -> String
format = leftPad . show

leftPad :: String -> String
leftPad xs = case xs of
                  [_]   -> "0" ++ xs
                  xs  -> xs

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock h m) = fromHourMin (hour + h) (min + m)
