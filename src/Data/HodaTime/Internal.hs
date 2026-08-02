module Data.HodaTime.Internal
(
   secondsFromSeconds
  ,secondsFromMinutes
  ,secondsFromHours
  ,clamp
)
where

import Data.HodaTime.Constants (secondsPerHour, secondsPerMinute)

-- conversion

secondsFromSeconds :: (Integral a, Num b) => a -> b
secondsFromSeconds = fromIntegral
{-# INLINE secondsFromSeconds #-}

secondsFromMinutes :: (Integral a, Num b) => a -> b
secondsFromMinutes = fromIntegral . (*secondsPerMinute)
{-# INLINE secondsFromMinutes #-}

secondsFromHours :: (Integral a, Num b) => a -> b
secondsFromHours = fromIntegral . (*secondsPerHour)
{-# INLINE secondsFromHours #-}

-- utility

clamp :: Ord a => a -> a -> a -> a
clamp small big = min big . max small
{-# INLINE clamp #-}
