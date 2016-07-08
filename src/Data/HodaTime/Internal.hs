module Data.HodaTime.Internal
(
   secondsFromSeconds
  ,secondsFromMinutes
  ,secondsFromHours
  ,hoursFromSecs
  ,minutesFromSecs
  ,secondsFromSecs
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

-- lenses

hoursFromSecs :: (Functor f, Num b, Integral b) => (b -> a) -> (Int -> f Int) -> b -> f a
hoursFromSecs to f secs = unitFromSeconds to h r (*secondsPerHour) f
  where
    h = secs `div` secondsPerHour
    r = secs - (h*secondsPerHour)
{-# INLINE hoursFromSecs #-}

minutesFromSecs :: (Functor f, Num b, Integral b) => (b -> a) -> (Int -> f Int) -> b -> f a
minutesFromSecs to f secs = unitFromSeconds to m r (*60) f
  where
    m = secs `mod` secondsPerHour `div` 60
    r = secs - (m*60)
{-# INLINE minutesFromSecs #-}

secondsFromSecs :: (Functor f, Num b, Integral b) => (b -> a) -> (Int -> f Int) -> b -> f a
secondsFromSecs to f secs = unitFromSeconds to s r id f
  where
    s = secs `mod` 60
    r = secs - s
{-# INLINE secondsFromSecs #-}

-- utility

clamp :: Ord a => a -> a -> a -> a
clamp small big = min big . max small
{-# INLINE clamp #-}

-- helper functions

unitFromSeconds :: (Functor f, Num b, Integral b) => (b -> a) -> b -> b -> (b -> b) -> (Int -> f Int) -> f a
unitFromSeconds to unit rest fromSecs f = to . (rest+) . fromSecs . fromIntegral <$> f (fromIntegral unit)
{-# INLINE unitFromSeconds #-}
