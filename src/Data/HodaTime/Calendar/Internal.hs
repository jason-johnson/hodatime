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

import Data.HodaTime.CalendarDateTime.Internal (CalendarDate(..), IsCalendar(..), Year, DayOfMonth)
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

mkCommonDayLens :: (IsCalendar cal, Functor f, Enum (Month cal)) =>
     Int
  -> (Year -> Month cal -> DayOfMonth -> Int)
  -> (Int32 -> (Word32, Word8, Word8))
  -> (DayOfMonth -> f DayOfMonth)
  -> CalendarDate cal
  -> f (CalendarDate cal)
mkCommonDayLens preStartDay yearMonthDayToDays daysToyearMonthDays f (CalendarDate _ d m y) = mkcd . (rest+) <$> f (fromIntegral d)
    where
      rest = pred $ yearMonthDayToDays (fromIntegral y) (toEnum . fromIntegral $ m) 1
      mkcd days =
        let
          days' = fromIntegral $ if days > preStartDay then days else preStartDay + 1
          (y', m', d') = daysToyearMonthDays days'
        in CalendarDate days' d' m' y'
{-# INLINE mkCommonDayLens #-}

mkCommonMonthLens :: (IsCalendar cal, Functor f, Enum (Month cal)) =>
     (Int, Int, Word8)
  -> (Month cal -> Year -> Int)
  -> (Year -> Month cal -> DayOfMonth -> Int)
  -> (Int -> f Int)
  -> CalendarDate cal
  -> f (CalendarDate cal)
mkCommonMonthLens firstDayTuple maxDaysInMonth yearMonthDayToDays f (CalendarDate _ d m y) = mkcd <$> f (fromEnum m)
    where
      mkcd months = CalendarDate (fromIntegral days) d'' (fromIntegral m') (fromIntegral y'')
        where
          (y', months') = flip divMod 12 >>> first (+ fromIntegral y) $ months
          (y'', m', d') = if (y', months', d) < firstDayTuple then firstDayTuple else (y', months', d)
          mdim = fromIntegral $ maxDaysInMonth (toEnum m') y'
          d'' = if d' > mdim then mdim else d'
          days = yearMonthDayToDays y'' (toEnum m') (fromIntegral d'')
{-# INLINE mkCommonMonthLens #-}

mkYearLens :: (IsCalendar cal, Functor f, Enum (Month cal)) =>
     (Int, Word8, Word8)
  -> (Month cal -> Year -> Int)
  -> (Year -> Month cal -> DayOfMonth -> Int)
  -> (Int -> f Int)
  -> CalendarDate cal
  -> f (CalendarDate cal)
mkYearLens firstDayTuple maxDaysInMonth yearMonthDayToDays f (CalendarDate _ d m y) = mkcd <$> f (fromIntegral y)
    where
      mkcd y' = CalendarDate days d'' m' (fromIntegral y'')
        where
          (y'', m', d') = if (y', m, d) < firstDayTuple then firstDayTuple else (y', m, d)
          m'' = toEnum . fromIntegral $ m'
          mdim = fromIntegral $ maxDaysInMonth m'' y''
          d'' = if d' > mdim then mdim else d'
          days = fromIntegral $ yearMonthDayToDays y'' m'' (fromIntegral d'')        
{-# INLINE mkYearLens #-}

moveByDow :: (IsCalendar cal, Enum (DayOfWeek cal)) =>
     (Int32 -> (Word32, Word8, Word8))
  -> DayOfWeek cal
  -> Int
  -> DayOfWeek cal
  -> (Int -> Int -> Int)
  -> (Int -> Int -> Int)
  -> (Int -> Int -> Bool)
  -> Int
  -> CalendarDate cal
moveByDow daysToYearMonthDay epochDayOfWeek n dow distanceF adjust cmp days = CalendarDate days' d m y
  where
    n' = if targetDow `cmp` currentDoW then n-1 else n
    currentDoW = dayOfWeekFromDays epochDayOfWeek days
    targetDow = fromIntegral . fromEnum $ dow
    distance = distanceF targetDow currentDoW
    days' = fromIntegral $ fromIntegral days `adjust` (7 * n') `adjust` distance
    (y, m, d) = daysToYearMonthDay days'

dayOfWeekFromDays :: (IsCalendar cal, Enum (DayOfWeek cal)) => DayOfWeek cal -> Int -> Int
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