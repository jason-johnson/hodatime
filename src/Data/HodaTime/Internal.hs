module Data.HodaTime.Internal
(
   secondsFromSeconds
  ,secondsFromMinutes
  ,secondsFromHours
)
where

import Data.HodaTime.Constants (secondsPerHour, secondsPerMinute)

secondsFromSeconds :: (Integral a, Num b) => a -> b
secondsFromSeconds = fromIntegral
{-# INLINE secondsFromSeconds #-}

secondsFromMinutes :: (Integral a, Num b) => a -> b
secondsFromMinutes = fromIntegral . (*secondsPerMinute)
{-# INLINE secondsFromMinutes #-}

secondsFromHours :: (Integral a, Num b) => a -> b
secondsFromHours = fromIntegral . (*secondsPerHour)
{-# INLINE secondsFromHours #-}
