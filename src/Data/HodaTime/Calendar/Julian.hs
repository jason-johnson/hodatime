{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Calendar.Julian
-- Copyright   :  (C) 2017 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- This is the module for 'CalendarDate' and 'CalendarDateTime' in the 'Julian' calendar.  The Julian calendar has a simple leap year rule \- every fourth year is a leap year, with none of the
-- century exceptions that 'Data.HodaTime.Calendar.Gregorian' later added to keep the calendar aligned to the solar year.  It is proleptic in that, while it only started in 45 BC, this
-- implementation applies that rule uniformly and does not try to account for the fact that before around 4 AD the leap year rule was accidentally implemented as a leap year every three years.  This
-- implementation stores the year unsigned, so its supported range is AD 1 onward (BC years are not representable).  Dates share the same absolute timeline as every other calendar, so in the modern
-- era a Julian date reads 13 days behind the same instant's Gregorian date.
----------------------------------------------------------------------------
module Data.HodaTime.Calendar.Julian
(
  -- * Constructors
   calendarDate
  ,fromNthDay
  ,fromWeekDate
  -- * Types
  ,Month(..)
  ,DayOfWeek(..)
  ,Julian
)
where

import Data.HodaTime.CalendarDateTime.Internal (IsCalendar(..), IsCalendarDateTime(..), CalendarDate, DayNth, DayOfMonth, Year, WeekNumber, CalendarDateTime(..), LocalTime(..), Date)
import Data.HodaTime.Instant.Internal (Instant(..))
import Data.HodaTime.Calendar.Internal (mkCommonDayLens, mkCommonMonthLens, mkYearLens, mkFromNthDay, mkFromWeekDate, moveByDow, dayOfWeekFromDays, commonMonthDayOffsets, borders, daysPerStandardYear, daysPerFourYears)
import Data.Int (Int32)
import Data.Word (Word8)
import Control.Arrow ((>>>), (***), (&&&))
import Control.Monad (guard)
import Data.Maybe (fromJust)
import Data.List (findIndex)

-- constants

-- | The Julian calendar predates the Gregorian one, so \- unlike 'Data.HodaTime.Calendar.Gregorian', which is only
--   valid from 15.Oct.1582 \- there is no reason to reject earlier dates here: rejecting pre\-1582 dates is exactly
--   what the Julian calendar exists to represent.  We floor at 1.Jan.AD 1 because the decoded year is stored unsigned
--   ('toYmd' returns a 'Word32' year), so BC years are not representable in this implementation.
firstJulDayTuple :: (Integral a, Integral b, Integral c) => (a, b, c)
firstJulDayTuple = (1, 0, 1)        -- NOTE: 1.Jan.AD 1

invalidDayThresh :: Integral a => a
invalidDayThresh = fromIntegral $ pred day0
  where
    (y, m, d) = firstJulDayTuple :: (Year, Int, DayOfMonth)
    day0 = yearMonthDayToDays y (toEnum m) d

epochDayOfWeek :: DayOfWeek Julian
epochDayOfWeek = Wednesday

-- | The Julian and (proleptic) Gregorian calendars diverge, so they cannot share a flat day 0 that is a clean date in
--   both.  'Data.HodaTime.Calendar.Gregorian' owns the shared absolute epoch (flat day 0 = 1.Mar.2000 Gregorian), which
--   the Julian calendar labels 17.Feb.2000.  Julian's own clean epoch (1.Mar.2000 Julian) sits 13 days later on that
--   shared timeline, so we shift the internal Julian day count by this constant to place it on the same absolute
--   timeline as every other calendar.  Because it is the gap between two fixed absolute days, the shift is constant for
--   all of time.
julianEpochShift :: Num a => a
julianEpochShift = 13

-- In case we ever decide to generate a 28 year table to store cycles
-- daysPerSolarCycle :: Num a => a
-- daysPerSolarCycle = 10227     -- NOTE: 28 Julian years = 10227 days = 1461 * 7 weeks exactly (dates and weekdays repeat)

-- types
    
data Julian
    
instance IsCalendar Julian where
  data Date Julian = JulianDate {-# UNPACK #-} !Int32 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Int32
    deriving (Eq, Show, Ord)

  data DayOfWeek Julian = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

  data Month Julian = January | February | March | April | May | June | July | August | September | October | November | December
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

  fromDays = julianFromDays
  toDays = julianToDays
  toYmd = julianToYmd

  day' = mkCommonDayLens invalidDayThresh yearMonthDayToDays julianFromDays julianToYmd
  {-# INLINE day' #-}

  month' (JulianDate _ _ m _) = toEnum . fromIntegral $ m

  monthl' = mkCommonMonthLens 12 firstJulDayTuple maxDaysInMonth yearMonthDayToDays julianToYmd julianFromDays
  {-# INLINE monthl' #-}

  year' = mkYearLens firstJulDayTuple maxDaysInMonth yearMonthDayToDays julianToYmd julianFromDays
  {-# INLINE year' #-}

  dayOfWeek' (JulianDate days _ _ _) = toEnum . dayOfWeekFromDays epochDayOfWeek . fromIntegral $ days

  next' n dow (JulianDate days _ _ _) = moveByDow julianFromDays epochDayOfWeek n dow (-) (+) (>) (fromIntegral days)

  previous' n dow (JulianDate days _ _ _) = moveByDow julianFromDays epochDayOfWeek n dow subtract (-) (<) (fromIntegral days)  -- NOTE: subtract is (-) with the arguments flipped

instance IsCalendarDateTime Julian where
  fromAdjustedInstant (Instant days secs nsecs) = CalendarDateTime (julianFromDays days) (LocalTime secs nsecs)
  toUnadjustedInstant (CalendarDateTime jd (LocalTime secs nsecs)) = Instant (julianToDays jd) secs nsecs

-- | Build the flat Julian date (denormalized: keeps the day count plus the decoded day\/month\/year).
julianFromDays :: Int32 -> Date Julian
julianFromDays days = JulianDate days d m y
  where (y, m, d) = daysToYearMonthDay days

julianToDays :: Date Julian -> Int32
julianToDays (JulianDate days _ _ _) = days

julianToYmd :: Date Julian -> (Int32, Word8, Word8)
julianToYmd (JulianDate _ d m y) = (y, m, d)

-- Constructors

-- | Smart constructor for a 'Julian' calendar date.
calendarDate :: DayOfMonth -> Month Julian -> Year -> Maybe (CalendarDate Julian)
calendarDate d m y = do
  guard $ d > 0 && d <= maxDaysInMonth m y
  let days = fromIntegral $ yearMonthDayToDays y m d
  guard $ days > invalidDayThresh
  return $ julianFromDays days

-- | Smart constructor for a 'Julian' calendar date given as a day relative to a month (e.g. the third Monday of the month).  Returns 'Nothing' if the resulting date is invalid.
fromNthDay :: DayNth -> DayOfWeek Julian -> Month Julian -> Year -> Maybe (CalendarDate Julian)
fromNthDay = mkFromNthDay invalidDayThresh epochDayOfWeek yearMonthDayToDays maxDaysInMonth julianFromDays

-- | Smart constructor for a 'Julian' calendar date given as a week date.  Note that this method assumes weeks start on Sunday and the first week of the year is the one
--   which has at least one day in the new year.
fromWeekDate :: WeekNumber -> DayOfWeek Julian -> Year -> Maybe (CalendarDate Julian)
fromWeekDate = mkFromWeekDate invalidDayThresh epochDayOfWeek yearMonthDayToDays julianFromDays 1 Sunday

-- helper functions

maxDaysInMonth :: Month Julian -> Year -> Int
maxDaysInMonth February y
  | isLeap                                = 29
  | otherwise                             = 28
  where
    isLeap                                = 0 == y `mod` 4
maxDaysInMonth m _
  | m == April || m == June || m == September || m == November  = 30
  | otherwise                                                   = 31

yearMonthDayToDays :: Year -> Month Julian -> DayOfMonth -> Int
yearMonthDayToDays y m d = days
  where
    m' = if m > February then fromEnum m - 2 else fromEnum m + 10
    years = if m < March then y - 2001 else y - 2000
    yearDays = years * daysPerStandardYear + years `div` 4
    days = yearDays + commonMonthDayOffsets !! m' + d - 1 + julianEpochShift

daysToYearMonthDay :: Int32 -> (Int32, Word8, Word8)
daysToYearMonthDay days0 = (fromIntegral y, fromIntegral m'', fromIntegral d')
  where
    days = days0 - julianEpochShift
    (fourYears, (remaining, isLeapDay)) = flip divMod daysPerFourYears >>> (* 4) *** id &&& borders daysPerFourYears $ days
    (oneYears, yearDays) = remaining `divMod` daysPerStandardYear
    -- NOTE: the sentinel 'daysPerStandardYear' lets February (yearDays >= the last real offset) be found; without it
    -- 'findIndex' returns Nothing and 'fromJust' crashes for any late-February date.
    m = pred . fromJust . findIndex (\mo -> yearDays < mo) $ commonMonthDayOffsets ++ [daysPerStandardYear]
    (m', startDate) = if m >= 10 then (m - 10, 2001) else (m + 2, 2000)
    d = yearDays - commonMonthDayOffsets !! m + 1
    (m'', d') = if isLeapDay then (1, 29) else (m', d)
    y = startDate + fourYears + oneYears