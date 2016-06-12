module Data.HodaTime.Internal
(
   fromSeconds
  ,fromMinutes
  ,fromHours
)
where

import Data.HodaTime.Constants (secondsPerHour, secondsPerMinute)

fromSeconds :: (Integral a, Num b) => a -> b
fromSeconds = fromIntegral

fromMinutes :: (Integral a, Num b) => a -> b
fromMinutes = fromIntegral . (*secondsPerMinute)

fromHours :: (Integral a, Num b) => a -> b
fromHours = fromIntegral . (*secondsPerHour)
