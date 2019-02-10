module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock {
   hours :: Int,
   minutes :: Int
} deriving (Eq)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min
  | (abs min) >= 60  = fromHourMin (hour + min `div` 60) (min `mod` 60)
  | (abs hour) >= 24 = fromHourMin (hour `mod` 24) min
  | min < 0          = fromHourMin (hour - 1) (60 + min)
  | hour < 0         = fromHourMin (24 + hour) min
  | otherwise        = Clock hour min

instance Show Clock where
  show c = (format $ hours c) ++ ":" ++ (format $ minutes c)
           where
             format     = leftPad . show
             leftPad xs = case xs of
                               [_]   -> "0" ++ xs
                               xs    -> xs

toString :: Clock -> String
toString = show

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock h m) = fromHourMin (hour + h) (min + m)
