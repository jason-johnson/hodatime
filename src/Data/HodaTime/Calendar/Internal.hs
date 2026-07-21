{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.HodaTime.Calendar.Internal
(
   mkCommonDayLens
  ,mkCommonMonthLens
  ,mkYearLens
  ,moveByDow
  ,dayOfWeekFromDays
  ,commonMonthDayOffsets
  ,borders
  ,daysPerStandardYear
  ,daysPerFourYears
  ,daysPerCentury
)
where

import Data.HodaTime.CalendarDateTime.Internal (Year, DayOfMonth)
import Data.Int (Int32)
import Data.Word (Word8, Word32)
import Control.Arrow ((>>>), first)

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
  -> (d -> (Word32, Word8, Word8))
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
     (Int, Int, Word8)
  -> (mon -> Year -> Int)
  -> (Year -> mon -> DayOfMonth -> Int)
  -> (d -> (Word32, Word8, Word8))
  -> (Int32 -> d)
  -> (Int -> f Int)
  -> d
  -> f d
mkCommonMonthLens firstDayTuple maxDaysInMonth yearMonthDayToDays toYmd fromDays f date = mkcd <$> f (fromIntegral m)
    where
      (y, m, d) = toYmd date
      mkcd months = fromDays (fromIntegral days)
        where
          (y', months') = flip divMod 12 >>> first (+ fromIntegral y) $ months
          (y'', m', d') = if (y', months', d) < firstDayTuple then firstDayTuple else (y', months', d)
          mdim = fromIntegral $ maxDaysInMonth (toEnum m') y'
          d'' = if d' > mdim then mdim else d'
          days = yearMonthDayToDays y'' (toEnum m') (fromIntegral d'')
{-# INLINE mkCommonMonthLens #-}

mkYearLens :: (Functor f, Enum mon) =>
     (Int, Word8, Word8)
  -> (mon -> Year -> Int)
  -> (Year -> mon -> DayOfMonth -> Int)
  -> (d -> (Word32, Word8, Word8))
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