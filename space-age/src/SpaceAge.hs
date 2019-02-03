module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = (/ yearRatioToEarth planet) $ secondsToEarthYears seconds
  where
    yearRatioToEarth p = case p of
       Mercury -> 0.2408467
       Venus -> 0.61519726
       Earth -> 1
       Mars -> 1.8808158
       Jupiter -> 11.862615
       Saturn -> 29.4479498
       Uranus -> 84.016846
       Neptune -> 164.79132
    secondsToEarthYears s =
      let secondsInADay   = 24 * 60 * 60
          secondsInAYear  = 365.25 * secondsInADay
      in  s / secondsInAYear

