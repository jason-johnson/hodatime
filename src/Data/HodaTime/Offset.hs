module Data.HodaTime.Offset
(
   seconds
  ,minutes
  ,hours
  ,add
  ,minus
)
where

import Data.HodaTime.OffsetDateTime.Internal
import Data.HodaTime.Constants (secondsPerHour)

-- Offset specific constants

maxOffsetHours :: Num a => a
maxOffsetHours = 18

minOffsetHours :: Num a => a
minOffsetHours = negate maxOffsetHours

maxOffsetSeconds :: Num a => a
maxOffsetSeconds = maxOffsetHours * secondsPerHour

minOffsetSeconds :: Num a => a
minOffsetSeconds = negate maxOffsetSeconds

maxOffsetMinutes :: Num a => a
maxOffsetMinutes = maxOffsetHours * 60

minOffsetMinutes :: Num a => a
minOffsetMinutes = negate maxOffsetMinutes

offsetError :: Show a => a -> t
offsetError i = error $ (show i) ++ " is outside of valid offset range"

-- public interface

-- | Create an 'Offset' of s seconds
seconds :: Int -> Offset
seconds s
    | s <= maxOffsetSeconds && s >= minOffsetSeconds = Offset . fromIntegral $ s
    | otherwise = offsetError s

-- | Create an 'Offset' of m minutes
minutes :: Int -> Offset
minutes m
    | m <= maxOffsetMinutes && m >= minOffsetMinutes = Offset . fromIntegral $ m * 60
    | otherwise = offsetError m

-- | Create an 'Offset' of h hours
hours :: Int -> Offset
hours h
    | h <= maxOffsetHours && h >= minOffsetHours = Offset . fromIntegral $ h * secondsPerHour
    | otherwise = offsetError h

-- math

add :: Offset -> Offset -> Offset
add (Offset lsecs) (Offset rsecs) = seconds . fromIntegral $ lsecs + rsecs

minus :: Offset -> Offset -> Offset
minus (Offset lsecs) (Offset rsecs) = seconds . fromIntegral $ lsecs - rsecs