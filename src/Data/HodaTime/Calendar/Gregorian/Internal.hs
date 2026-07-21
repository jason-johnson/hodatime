{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.HodaTime.Calendar.Gregorian.Internal
(
   daysToYearMonthDay
  ,fromWeekDate
  ,Gregorian
  ,Month(..)
  ,DayOfWeek(..)
  ,invalidDayThresh
  ,epochDayOfWeek
  ,maxDaysInMonth
  ,yearMonthDayToDays
  ,nthDayToDayOfMonth
  ,dayOfWeekFromDays
  ,instantToYearMonthDay
  ,yearMonthDayToCycleCenturyDays
  ,gregorianFromYmd
  ,gregorianToDays
  ,daysToGregorian
  ,gregorianToYearMonthDay
)
where

import Data.HodaTime.CalendarDateTime.Internal (IsCalendar(..), IsCalendarDateTime(..), DayOfMonth, Year, WeekNumber, CalendarDateTime(..), LocalTime(..), Date)
import Data.HodaTime.Calendar.Gregorian.CacheTable (DTCacheTable(..), decodeMonth, decodeYear, decodeDay, cacheTable)
import Data.HodaTime.Calendar.Internal (mkCommonMonthLens, mkYearLens, dayOfWeekFromDays, commonMonthDayOffsets, borders, daysPerStandardYear, daysPerCentury)
import Data.HodaTime.Instant.Internal (Instant(..))
import Control.Arrow ((>>>), (&&&), (***), first)
import Data.Int (Int32, Int8)
import Data.Word (Word8, Word32)
import Data.Array.Unboxed ((!))
import Control.Monad (guard)

-- Constants

yearsPerCycle :: Num a => a
yearsPerCycle = 400

daysPerCycle :: Num a => a      -- NOTE: A "cycle" is 400 years
daysPerCycle = 146097

invalidDayThresh :: Integral a => a
invalidDayThresh = -152445      -- NOTE: 14.Oct.1582, one day before Gregorian calendar came into effect

firstGregDayTuple :: (Integral a, Integral b, Integral c) => (a, b, c)
firstGregDayTuple = (1582, 9, 15)
    
epochDayOfWeek :: DayOfWeek Gregorian
epochDayOfWeek = Wednesday

-- types
    
data Gregorian
    
instance IsCalendar Gregorian where
  data Date Gregorian = GregorianDate {-# UNPACK #-} !Int8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word32
    deriving (Eq, Show, Ord)

  data DayOfWeek Gregorian = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

  data Month Gregorian = January | February | March | April | May | June | July | August | September | October | November | December
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

  fromDays = daysToGregorian
  toDays = gregorianToDays
  toYmd = gregorianToYearMonthDay

  -- Fast path: shift only the day-in-century, leaving cycle\/century untouched when we stay in-century.
  day' f gd = mkgd <$> f (fromIntegral d)
    where
      (_, _, d) = gregorianToYearMonthDay gd
      mkgd d' = shiftDaysWith clampToValid (d' - fromIntegral d) gd

  month' gd = toEnum . fromIntegral $ m
    where (_, m, _) = gregorianToYearMonthDay gd

  monthl' = mkCommonMonthLens firstGregDayTuple maxDaysInMonth yearMonthDayToDays gregorianToYearMonthDay daysToGregorian
  {-# INLINE monthl' #-}

  year' = mkYearLens firstGregDayTuple maxDaysInMonth yearMonthDayToDays gregorianToYearMonthDay daysToGregorian
  {-# INLINE year' #-}

  dayOfWeek' (GregorianDate _ century dic) = toEnum . dayOfWeekFromDays epochDayOfWeek $ 5 * fromIntegral century + fromIntegral dic

  next' n dow gd@(GregorianDate _ century dic) = shiftDaysWith id (7 * n' + targetDow - currentDoW) gd
    where
      currentDoW = dayOfWeekFromDays epochDayOfWeek $ 5 * fromIntegral century + fromIntegral dic
      targetDow = fromEnum dow
      n' = if targetDow > currentDoW then n - 1 else n

  previous' n dow gd@(GregorianDate _ century dic) = shiftDaysWith id (negate $ 7 * n' + currentDoW - targetDow) gd
    where
      currentDoW = dayOfWeekFromDays epochDayOfWeek $ 5 * fromIntegral century + fromIntegral dic
      targetDow = fromEnum dow
      n' = if targetDow < currentDoW then n - 1 else n

instance IsCalendarDateTime Gregorian where
  fromAdjustedInstant (Instant days secs nsecs) = CalendarDateTime (daysToGregorian days) (LocalTime secs nsecs)
  toUnadjustedInstant (CalendarDateTime gd (LocalTime secs nsecs)) = Instant (gregorianToDays gd) secs nsecs

-- constructors

fromWeekDate :: Int -> DayOfWeek Gregorian -> WeekNumber -> DayOfWeek Gregorian -> Year -> Maybe (Date Gregorian)
fromWeekDate minWeekDays wkStartDoW weekNum dow y = do
  guard $ days > invalidDayThresh
  return $ daysToGregorian days
    where
      soyDays = yearMonthDayToDays y January minWeekDays
      soyDoW = dayOfWeekFromDays epochDayOfWeek soyDays
      startDoWDistance = fromEnum soyDoW - fromEnum wkStartDoW
      dowDistance = fromEnum dow - fromEnum wkStartDoW
      dowDistance' = if dowDistance < 0 then dowDistance + 7 else dowDistance
      startDays = soyDays - startDoWDistance
      weekNum' = pred weekNum
      days = fromIntegral $ startDays + weekNum' * 7 + dowDistance'

-- helper functions

nthDayToDayOfMonth :: Int -> Int -> Month Gregorian -> Int -> Int
nthDayToDayOfMonth nth day month y = dom + d' + 7 * nth
  where
    mdm = maxDaysInMonth month y
    dom = if nth < 0 then mdm else 1
    m = fromEnum month
    dow = (dom + (13 * m' - 1) `div` 5 + yrhs + (yrhs `div` 4) + (ylhs `div` 4) - 2 * ylhs) `mod` 7
    d = day - dow
    d' = if d < 0 then d + 7 else d
    (m', y') = if m < 2 then (m + 11, y - 1) else (m - 1, y)
    yrhs = y' `mod` 100
    ylhs = y' `div` 100

maxDaysInMonth :: Month Gregorian -> Year -> Int
maxDaysInMonth February y
  | isLeap                                = 29
  | otherwise                             = 28
  where
    isLeap
      | 0 == y `mod` 100                  = 0 == y `mod` 400
      | otherwise                         = 0 == y `mod` 4
maxDaysInMonth m _
  | m == April || m == June || m == September || m == November  = 30
  | otherwise                                                   = 31

-- | Construct the (cycle, century, day-in-century) triple directly from a year\/month\/day, without first
--   computing the flat day count and dividing it back down.  Within a cycle each century is exactly 36524 days
--   (4*36524 = 146097 - 1, the missing day being the cycle's extra leap day), and within a century the leap rule
--   reduces to a plain \/4 (the \/100 and \/400 corrections vanish for year-offsets 0..99).  This naturally yields
--   representation (ii): 'century' is always in [0,3] and the extra leap day falls out as day 36524 of the last century.
yearMonthDayToCycleCenturyDays :: Year -> Month Gregorian -> DayOfMonth -> (Int, Int, Int)
yearMonthDayToCycleCenturyDays y m d = (cyc, century, dic)
  where
    years = if m < March then y - 2001 else y - 2000
    (cyc, yearInCycle) = years `divMod` yearsPerCycle
    (century, yoc) = yearInCycle `divMod` 100
    m' = if m > February then fromEnum m - 2 else fromEnum m + 10
    dic = yoc * daysPerStandardYear + yoc `div` 4 + commonMonthDayOffsets !! m' + d - 1

-- | Build a 'Date' 'Gregorian' directly from a year\/month\/day via 'yearMonthDayToCycleCenturyDays' (keeping the
--   'GregorianDate' constructor internal to this module).  No validity checking is performed here.
gregorianFromYmd :: Year -> Month Gregorian -> DayOfMonth -> Date Gregorian
gregorianFromYmd y m d = GregorianDate (fromIntegral cyc) (fromIntegral century) (fromIntegral dic)
  where (cyc, century, dic) = yearMonthDayToCycleCenturyDays y m d

-- NOTE: Epoch is March 1 2000 because that has nicest properties that is near our current time.
-- TODO: The addition of leap days below will add from the previous year.  We need to determine if this is a bug
-- TODO: and if it is not, why isn't it
yearMonthDayToDays :: Year -> Month Gregorian -> DayOfMonth -> Int
yearMonthDayToDays y m d = days
  where
    m' = if m > February then fromEnum m - 2 else fromEnum m + 10
    years = if m < March then y - 2001 else y - 2000
    yearDays = years * daysPerStandardYear + years `div` 4 + years `div` 400 - years `div` 100
    days = yearDays + commonMonthDayOffsets !! m' + d - 1
  
-- | Count up centuries, plus remaining days and determine if this is a special extra cycle day.  NOTE: This
--   function would be more accurate if it only took absolute values, but it does end up coming up with the correct answer even on negatives.  It just
--   ends up doing extra calculations with negatives (e.g. year comes back as -100 and entry is +100, which ends up being right but it could have been 0 and the +0 entry)
calculateCenturyDays :: Int32 -> (Int32, Int32, Bool)
calculateCenturyDays days = (y, centuryDays, isExtraCycleDay)
  where
    (cycleYears, (cycleDays, isExtraCycleDay)) = flip divMod daysPerCycle >>> (* 400) *** id &&& borders daysPerCycle $ days
    (centuryYears, centuryDays) = flip divMod daysPerCentury >>> first (* 100) $ cycleDays
    y = cycleYears + centuryYears

daysToYearMonthDay :: Int32 -> (Word32, Word8, Word8)
daysToYearMonthDay days = (fromIntegral y', m'', fromIntegral d')
  where
    (centuryYears, centuryDays, isExtraCycleDay) = calculateCenturyDays days
    decodeEntry (DTCacheTable xs _) = (\x -> (decodeYear x, decodeMonth x, decodeDay x)) . (!) xs
    (y,m,d) = decodeEntry cacheTable . fromIntegral $ centuryDays
    (m',d') = if isExtraCycleDay then (1,29) else (m,d)
    (y',m'') = (2000 + centuryYears + fromIntegral y, fromIntegral $ m')

-- here to avoid circular dependancy between Instant and Gregorian
instantToYearMonthDay :: Instant -> (Word32, Word8, Word8)
instantToYearMonthDay (Instant days _ _) = daysToYearMonthDay days

-- Date Gregorian bridge functions (cycle\/century\/day-in-century representation)

-- | Reconstruct the flat (epoch-relative) day count from a 'Date' 'Gregorian'.  Inverse of 'daysToGregorian'; must
--   agree with 'yearMonthDayToDays' so the cycle representation round-trips against the flat day count.
gregorianToDays :: Date Gregorian -> Int32
gregorianToDays (GregorianDate cyc century days) = fromIntegral $ cyc' * daysPerCycle + century' * daysPerCentury + days'
  where
    cyc' = fromIntegral cyc :: Int
    century' = fromIntegral century :: Int
    days' = fromIntegral days :: Int

-- | Decompose a flat (epoch-relative) day count into the cycle\/century\/day-in-century representation.  Uses floored
--   'divMod' so remainders are non-negative.  Representation (ii): 'century' is always in [0,3]; the single extra leap
--   day per cycle (which floored division would place at century 4, day 0) is folded back to day 36524 of century 3.
daysToGregorian :: Int32 -> Date Gregorian
daysToGregorian days = GregorianDate (fromIntegral cycles) (fromIntegral century) (fromIntegral dic)
  where
    (cycles, cycleDays) = (fromIntegral days :: Int) `divMod` daysPerCycle
    (century0, dic0) = cycleDays `divMod` daysPerCentury
    (century, dic) = if century0 == (4 :: Int) then (3, dic0 + daysPerCentury) else (century0, dic0)

-- | Decode a 'Date' 'Gregorian' directly to (year, zero-based month, day) from its stored fields: the cycle\/century
--   split is already present, so month and day come from a single cache-table lookup on the day-in-century.
gregorianToYearMonthDay :: Date Gregorian -> (Word32, Word8, Word8)
gregorianToYearMonthDay (GregorianDate cyc century dic)
  | dic == daysPerCentury = (fromIntegral extraYear, 1, 29)   -- extra-cycle-day: 29 Feb (month 1 = February, 0-based)
  | otherwise             = (fromIntegral yr, fromIntegral m, fromIntegral d)
  where
    cycleYear = fromIntegral cyc * (400 :: Int)
    extraYear = 2000 + cycleYear + 400
    yr = 2000 + cycleYear + fromIntegral century * 100 + fromIntegral y
    (y, m, d) = decodeEntry cacheTable . fromIntegral $ dic
    decodeEntry (DTCacheTable xs _) = (\x -> (decodeYear x, decodeMonth x, decodeDay x)) . (!) xs

-- | Shift a date by 'delta' days.  Fast path: when the shift stays within the current century (and we are safely
--   past the pre-Gregorian threshold, so cyc >= -1) only the day-in-century changes and the cycle\/century are
--   untouched.  Otherwise fall back to reconstructing the flat day count, applying 'onFlat' (e.g. the validity clamp),
--   and re-decomposing.  The extra-cycle-day (day-in-century == 36524) always fails the in-century bound.
shiftDaysWith :: (Int32 -> Int32) -> Int -> Date Gregorian -> Date Gregorian
shiftDaysWith onFlat delta gd@(GregorianDate cyc century dic)
  | cyc >= -1 && dic' >= 0 && dic' < daysPerCentury = GregorianDate cyc century (fromIntegral dic')
  | otherwise                                       = daysToGregorian . onFlat $ gregorianToDays gd + fromIntegral delta
  where dic' = fromIntegral dic + delta :: Int

-- | Clamp a flat day count so it never precedes the first valid Gregorian date (15 Oct 1582).
clampToValid :: Int32 -> Int32
clampToValid days = if days > invalidDayThresh then days else invalidDayThresh + 1