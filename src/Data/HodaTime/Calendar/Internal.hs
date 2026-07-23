{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.HodaTime.Calendar.Internal
(
   mkCommonDayLens
  ,mkCommonMonthLens
  ,mkYearLens
  ,mkFromNthDay
  ,mkFromWeekDate
  ,moveByDow
  ,dayOfWeekFromDays
  ,commonMonthDayOffsets
  ,borders
  ,daysPerStandardYear
  ,daysPerFourYears
  ,daysPerCentury
)
where

import Data.HodaTime.CalendarDateTime.Internal (Year, DayOfMonth, DayNth, WeekNumber)
import Data.Int (Int32)
import Data.Word (Word8)
import Control.Arrow ((>>>), first)
import Control.Monad (guard)

-- Constants

daysPerStandardYear :: Num a => a
daysPerStandardYear = 365

daysPerFourYears :: Num a => a
daysPerFourYears = 1461

daysPerCentury :: Num a => a
daysPerCentury = 36524

-- helper functions
--
-- NOTE: These are representation-agnostic: each calendar passes its own @fromDays@ (build a date from a flat
-- epoch-relative day count) and @toYmd@ (decode a date to year\/month\/day) so the same lens logic works whether
-- the calendar stores a flat day count, a packed cycle, or anything else.

mkCommonDayLens :: (Functor f, Enum mon) =>
     Int
  -> (Year -> mon -> DayOfMonth -> Int)
  -> (Int32 -> d)
  -> (d -> (Int32, Word8, Word8))
  -> (DayOfMonth -> f DayOfMonth)
  -> d
  -> f d
mkCommonDayLens preStartDay yearMonthDayToDays fromDays toYmd f date = mkcd . (rest +) <$> f (fromIntegral d)
    where
      (y, m, d) = toYmd date
      rest = pred $ yearMonthDayToDays (fromIntegral y) (toEnum . fromIntegral $ m) 1
      mkcd days = fromDays days'
        where days' = fromIntegral $ if days > preStartDay then days else preStartDay + 1
{-# INLINE mkCommonDayLens #-}

mkCommonMonthLens :: (Functor f, Enum mon) =>
     Int
  -> (Int, Int, Word8)
  -> (mon -> Year -> Int)
  -> (Year -> mon -> DayOfMonth -> Int)
  -> (d -> (Int32, Word8, Word8))
  -> (Int32 -> d)
  -> (Int -> f Int)
  -> d
  -> f d
mkCommonMonthLens monthsPerYear firstDayTuple maxDaysInMonth yearMonthDayToDays toYmd fromDays f date = mkcd <$> f (fromIntegral m)
    where
      (y, m, d) = toYmd date
      mkcd months = fromDays (fromIntegral days)
        where
          (y', months') = flip divMod monthsPerYear >>> first (+ fromIntegral y) $ months
          (y'', m', d') = if (y', months', d) < firstDayTuple then firstDayTuple else (y', months', d)
          mdim = fromIntegral $ maxDaysInMonth (toEnum m') y'
          d'' = if d' > mdim then mdim else d'
          days = yearMonthDayToDays y'' (toEnum m') (fromIntegral d'')
{-# INLINE mkCommonMonthLens #-}

mkYearLens :: (Functor f, Enum mon) =>
     (Int, Word8, Word8)
  -> (mon -> Year -> Int)
  -> (Year -> mon -> DayOfMonth -> Int)
  -> (d -> (Int32, Word8, Word8))
  -> (Int32 -> d)
  -> (Int -> f Int)
  -> d
  -> f d
mkYearLens firstDayTuple maxDaysInMonth yearMonthDayToDays toYmd fromDays f date = mkcd <$> f (fromIntegral y)
    where
      (y, m, d) = toYmd date
      mkcd y' = fromDays days
        where
          (y'', m', d') = if (y', m, d) < firstDayTuple then firstDayTuple else (y', m, d)
          m'' = toEnum . fromIntegral $ m'
          mdim = fromIntegral $ maxDaysInMonth m'' y''
          d'' = if d' > mdim then mdim else d'
          days = fromIntegral $ yearMonthDayToDays y'' m'' (fromIntegral d'')
{-# INLINE mkYearLens #-}

-- | Build a date from the nth (or nth-from-last) weekday within a month (e.g. \"the third Monday\").  This is the
--   calendar-agnostic core of a per-calendar @fromNthDay@: it reads the weekday of the anchor day (the 1st, or the
--   last day of the month for a \"from last\" request) via the calendar's own day count, so it needs no per-calendar
--   weekday formula.
mkFromNthDay :: (Enum mon, Enum dow) =>
     Int                                   -- ^ invalid-day threshold (dates on or before this are rejected)
  -> dow                                   -- ^ epoch day of week
  -> (Year -> mon -> DayOfMonth -> Int)    -- ^ yearMonthDayToDays
  -> (mon -> Year -> Int)                  -- ^ maxDaysInMonth
  -> (Int32 -> d)                          -- ^ fromDays
  -> DayNth -> dow -> mon -> Year -> Maybe d
mkFromNthDay invalidDayThresh epochDayOfWeek yearMonthDayToDays maxDaysInMonth fromDays nth dow m y = do
  guard $ d > 0 && d <= mdim
  guard $ days > invalidDayThresh
  return $ fromDays (fromIntegral days)
    where
      nth' = fromEnum nth - 4
      mdim = maxDaysInMonth m y
      target = fromEnum dow
      -- forward (nth' >= 0) counts from the first of the month; \"from last\" (nth' < 0) counts back from the last day.
      -- Using the backward distance for the from-last case is what makes \"the last Friday\" land on the final Friday
      -- even when the month ends exactly on that weekday (where a naive forward offset would be a week short).
      d | nth' < 0  = mdim - backwardDist + 7 * (nth' + 1)
        | otherwise = 1    + forwardDist  + 7 * nth'
      forwardDist  = (target - dowOf 1)    `mod` 7
      backwardDist = (dowOf mdim - target) `mod` 7
      dowOf dom = dayOfWeekFromDays epochDayOfWeek (yearMonthDayToDays y m dom)
      days = yearMonthDayToDays y m d
{-# INLINE mkFromNthDay #-}

-- | Build a date from a week-numbering rule.  @minWeekDays@ and @weekStart@ define the rule (e.g. @1 Sunday@ for the
--   simple rule where week 1 is the first week with any day in the new year, or @4 Monday@ for ISO-8601).  This is the
--   calendar-agnostic core of a per-calendar @fromWeekDate@.
mkFromWeekDate :: (Enum mon, Enum dow) =>
     Int                                   -- ^ invalid-day threshold
  -> dow                                   -- ^ epoch day of week
  -> (Year -> mon -> DayOfMonth -> Int)    -- ^ yearMonthDayToDays
  -> (Int32 -> d)                          -- ^ fromDays
  -> Int                                   -- ^ minimum days of the new year that fall in week 1
  -> dow                                   -- ^ first day of the week
  -> WeekNumber -> dow -> Year -> Maybe d
mkFromWeekDate invalidDayThresh epochDayOfWeek yearMonthDayToDays fromDays minWeekDays wkStartDoW weekNum dow y = do
  guard $ days > invalidDayThresh
  return $ fromDays (fromIntegral days)
    where
      soyDays = yearMonthDayToDays y (toEnum 0) minWeekDays
      soyDoW = dayOfWeekFromDays epochDayOfWeek soyDays
      startDoWDistance = soyDoW - fromEnum wkStartDoW
      dowDistance = fromEnum dow - fromEnum wkStartDoW
      dowDistance' = if dowDistance < 0 then dowDistance + 7 else dowDistance
      startDays = soyDays - startDoWDistance
      days = startDays + pred weekNum * 7 + dowDistance'
{-# INLINE mkFromWeekDate #-}

moveByDow :: Enum dow =>
     (Int32 -> d)
  -> dow
  -> Int
  -> dow
  -> (Int -> Int -> Int)
  -> (Int -> Int -> Int)
  -> (Int -> Int -> Bool)
  -> Int
  -> d
moveByDow fromDays epochDayOfWeek n dow distanceF adjust cmp days = fromDays days'
  where
    n' = if targetDow `cmp` currentDoW then n-1 else n
    currentDoW = dayOfWeekFromDays epochDayOfWeek days
    targetDow = fromIntegral . fromEnum $ dow
    distance = distanceF targetDow currentDoW
    days' = fromIntegral $ fromIntegral days `adjust` (7 * n') `adjust` distance

dayOfWeekFromDays :: Enum dow => dow -> Int -> Int
dayOfWeekFromDays epochDayOfWeek = normalize . (fromEnum epochDayOfWeek +) . flip mod 7
  where
    normalize n = if n >= 7 then n - 7 else n

commonMonthDayOffsets :: Num a => [a]
commonMonthDayOffsets = 0 : rest
  where
    rest = zipWith (+) daysPerMonth (0:rest)
    daysPerMonth = [31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31]

-- | The issue is that 4 * daysPerCentury will be one less than daysPerCycle.  The reason for this is that the Gregorian calendar adds one more day per 400 year cycle
--   and this day is missing from adding up 4 individual centuries.  We have the same issue again with 4 years (i.e. 365*4 is daysPerFourYears - 1)
--   so we use this function to check if this has occurred so we can add the missing day back in.
borders :: (Num a, Eq a) => a -> a -> Bool
borders c x = x == c - 1