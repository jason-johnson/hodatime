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
import Data.HodaTime.Constants
import Data.HodaTime.CacheTable
import Data.Word (Word, Word16)
import Control.Arrow ((>>>), (&&&), (***), first)
import Data.List (findIndex, group, foldl')
import Data.Maybe (fromJust)

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

-- decoding

borders c x = x == c - 1

centuryDaysToDate days = (year, centuryDays, isLeapDay)
  where
    (cycleYears, (cycleDays, isLeapDay)) = flip divMod daysPerCycle >>> (* 400) *** id &&& borders daysPerCycle $ days
    (centuryYears, centuryDays) = flip divMod daysPerCentury >>> first (* 100) $ cycleDays
    year = cycleYears + centuryYears

daysToDate' :: Int -> (Int, Month, Int)
daysToDate' days = (year, fromInt month'', day')
  where
    (centuryYears, centuryDays, isLeapDay) = centuryDaysToDate days
    (fourYears, (remaining, isLeapDay')) = flip divMod daysPerFourYears >>> (* 4) *** id &&& borders daysPerFourYears $ centuryDays
    (oneYears, yearDays) = remaining `divMod` daysPerYear
    month = pred . fromJust . findIndex (\y -> yearDays < y) $ monthDayOffsets
    (month', startDate) = if month >= 10 then (month - 10, 2001) else (month + 2, 2000)
    day = yearDays - monthDayOffsets !! month + 1
    (month'', day') = if isLeapDay || isLeapDay' then (1, 29) else (month', day)
    year = startDate + centuryYears + fourYears + oneYears

daysToDate :: Int -> (Int, Month, Int)
daysToDate days = (y',m'', fromIntegral d')
  where
    (centuryYears, centuryDays, isLeapDay) = centuryDaysToDate days
    decodeEntry (DTCacheTable xs _ _) = (\x -> (decodeYear x, decodeMonth x, decodeDay x)) . (!!) xs
    (y,m,d) = decodeEntry cacheTable centuryDays
    (m',d') = if isLeapDay then (1,29) else (m,d)
    (y',m'') = (2000 + centuryYears + fromIntegral y, fromInt . fromIntegral $ m')

decodeDate :: Date -> (Int, Month, Int)
decodeDate (Date (DateTime days _ _)) = daysToDate days

decodeDateTime :: DateTime -> (Int, Month, Int, Word16, Word16, Word16, Word)
decodeDateTime (DateTime days secs nsecs) = (year, month, day, hour', minute, sec, nsecs)
  where
    (year, month, day) = daysToDate days
    decodeEntry (DTCacheTable _ _ xs) = (\x -> (decodeHour x, decodeMinute x, decodeSecond x)) . (!!) xs
    (secs', overTwelveHours) = if secs >= secondsInTwelveHours then (secs - secondsInTwelveHours, True) else (secs, False)
    (hour, minute, sec) = decodeEntry cacheTable . fromIntegral $ secs'
    hour' = if overTwelveHours then hour + 12 else hour   

decodeDate' :: Date -> (Int, Month, Int)
decodeDate' (Date (DateTime days _ _)) = daysToDate' days

decodeDateTime' :: DateTime -> (Int, Month, Int, Word, Word, Word, Word)
decodeDateTime' (DateTime days secs nsecs) = (year, month, day, hour, minute, sec, nsecs)
  where
    (year, month, day) = daysToDate' days
    (hour, secs') = secs `divMod` secondsPerHour
    (minute, sec) = secs' `divMod` minutesPerHour

testDaysImpl f = filter (\x -> length x /= 1) . group . map f $ enumFromToDate minBound maxBound
testDays' = testDaysImpl decodeDate'
testDays = testDaysImpl decodeDate

testYear year = (((cyy,rs),(cy,rs2),(y,ds)), ((cyy',rs'),(cy',rs2'),(y',ds')))
  where
    secsM = dtDays $ toDateTime year March 1 0 0 0 0
    secsF = dtDays $ toDateTime year February 29 0 0 0 0
    [(cyy,rs),(cyy',rs')] = (flip divMod daysPerCycle >>> first (* 400)) `fmap` [secsM, secsF]
    [(cy,rs2),(cy',rs2')] = (flip divMod daysPerCentury >>> first (* 100)) `fmap` [rs,rs']
    [(y,ds),(y',ds')] = (flip divMod daysPerFourYears >>> first (*4)) `fmap` [rs2,rs2']

testYears' = testYearsImpl decodeDate'
testYears = testYearsImpl decodeDate
testYearsImpl f = reverse . snd . foldl' g ((startYear,2,0),[]) . map f $ enumFromToDate minBound maxBound
  where
    startYear = 1600
    g ((y',m',d'), errors) (y,m,d) = if nextDay == curr then (curr,errors) else (curr,((y',m',d'),nextDay,curr,(y,m,d)):errors)
      where
        curr = (y,toInt m,d)
        nextDay
          | d' < maxDays = (y',m',d'+1)
          | m' < 11      = (y',m'+1,1)
          | otherwise    = (y'+1,0,1)
        months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        isLeap
          | 0 == y' `mod` 100 = 0 == y' `mod` 400
          | otherwise         = 0 == y' `mod` 4
        maxDays
          | m' == 1 && isLeap = 29
          | otherwise         = months !! m'
