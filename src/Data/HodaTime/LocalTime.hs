module Data.HodaTime.LocalTime
(
   fromHoursAndMinutes
  ,fromHMS
  ,hour
  ,minute
  ,second
)
where

import Data.HodaTime.LocalTime.Internal
import Data.HodaTime.Constants (secondsPerHour)
import Data.Word (Word32)

secsFromHours :: Int -> Word32
secsFromHours = (* secondsPerHour) . fromIntegral

secsFromMinutes :: Int -> Word32
secsFromMinutes = (* 60) . fromIntegral

-- Construction

fromHoursAndMinutes :: Int -> Int -> LocalTime
fromHoursAndMinutes h m = LocalTime (h' + m') 0
  where
    h' = secsFromHours h
    m' = secsFromMinutes m

fromHMS :: Int -> Int -> Int -> LocalTime
fromHMS h m s = LocalTime (h' + m' + fromIntegral s) 0
  where
    h' = secsFromHours h
    m' = secsFromMinutes m

-- Accessors

hour :: LocalTime -> Int
hour (LocalTime secs _) = fromIntegral h
  where
    h = secs `div` secondsPerHour

minute :: LocalTime -> Int
minute (LocalTime secs _) = fromIntegral m
  where
    s = secs `mod` secondsPerHour
    m = s `div` 60

second :: LocalTime -> Int
second (LocalTime secs _) = fromIntegral s'
  where
    s = secs `mod` secondsPerHour
    s' = s `mod` 60
