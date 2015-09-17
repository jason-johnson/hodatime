module Data.HodaTime.Calendar.Gregorian.Internal
(
  fromInstant
)
where

import Data.HodaTime.Constants (daysPerCycle, daysPerCentury, daysPerFourYears, daysPerYear, monthDayOffsets)
import Data.HodaTime.Instant.Internal (Instant(..))
import Data.HodaTime.Calendar(Calendar)
import Data.HodaTime.LocalDateTime.Internal(LocalDate(..))
import Control.Arrow ((>>>), (&&&), (***), first)
import Data.Maybe (fromJust)
import Data.List (findIndex)
import Data.HodaTime.Calendar.Gregorian.CacheTable (DTCacheTable(..), decodeMonth, decodeYear, decodeDay, cacheTable)
import Data.Int (Int32, Int8, Int16)

-- | Internal helper method to convert into a Gregorian compatible LocalDate
fromInstant :: Instant -> Calendar -> LocalDate
fromInstant (Instant days _ _) = LocalDate year month day
    where
        (year, month, day) = daysToYearMonthDay days

-- | The issue is that 4 * daysPerCentury will be one less than daysPerCycle.  The reason for this is that the Gregorian calendar adds one more day per 400 year cycle
--   and this day is missing from adding up 4 individual centuries.  We have the same issue again with 4 years (i.e. 365*4 is daysPerFourYears - 1)
--   so we use this function to check if this has occurred so we can add the missing day back in.
borders :: (Num a, Eq a) => a -> a -> Bool
borders c x = x == c - 1

calculateCenturyDays :: Int32 -> (Int32, Int32, Bool)
calculateCenturyDays days = (year, centuryDays, isExtraCycleDay)
  where
    (cycleYears, (cycleDays, isExtraCycleDay)) = flip divMod daysPerCycle >>> (* 400) *** id &&& borders daysPerCycle $ days
    (centuryYears, centuryDays) = flip divMod daysPerCentury >>> first (* 100) $ cycleDays
    year = cycleYears + centuryYears

daysToYearMonthDay :: Int32 -> (Int16, Int8, Int8)
daysToYearMonthDay days = (fromIntegral year, fromIntegral month'', fromIntegral day')
  where
    (centuryYears, centuryDays, isExtraCycleDay) = calculateCenturyDays days
    (fourYears, (remaining, isLeapDay)) = flip divMod daysPerFourYears >>> (* 4) *** id &&& borders daysPerFourYears $ centuryDays
    (oneYears, yearDays) = remaining `divMod` daysPerYear
    month = pred . fromJust . findIndex (\y -> yearDays < y) $ monthDayOffsets
    (month', startDate) = if month >= 10 then (month - 10, 2001) else (month + 2, 2000)
    day = yearDays - monthDayOffsets !! month + 1
    (month'', day') = if isExtraCycleDay || isLeapDay then (1, 29) else (month', day)
    year = startDate + centuryYears + fourYears + oneYears

-- TODO: At some point we should see how much a difference the caching makes
_daysToYearMonthDay' :: Int32 -> (Int32, Int8, Int8)
_daysToYearMonthDay' days = (y',m'', fromIntegral d')
  where
    (centuryYears, centuryDays, isExtraCycleDay) = calculateCenturyDays days
    decodeEntry (DTCacheTable xs _ _) = (\x -> (decodeYear x, decodeMonth x, decodeDay x)) . (!!) xs
    (y,m,d) = decodeEntry cacheTable . fromIntegral $ centuryDays
    (m',d') = if isExtraCycleDay then (1,29) else (m,d)
    (y',m'') = (2000 + centuryYears + fromIntegral y, fromIntegral $ m')