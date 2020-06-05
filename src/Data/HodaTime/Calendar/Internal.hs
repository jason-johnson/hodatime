{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.HodaTime.Calendar.Internal
(
   mkCommonDayLens
  ,mkCommonMonthLens
  ,mkYearLens
)
where

import Data.HodaTime.CalendarDateTime.Internal (CalendarDate(..), IsCalendar(..), Year, DayOfMonth)
import Data.Int (Int32)
import Data.Word (Word8, Word32)
import Control.Arrow ((>>>), first)

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