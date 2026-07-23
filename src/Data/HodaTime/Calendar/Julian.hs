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
-- This is the module for 'CalendarDate' and 'CalendarDateTime' in the 'Julian' calendar.  The Julian calendar has a simple leap year rule \- every fourth year is a leap year, with none of the century
-- exceptions that 'Data.HodaTime.Calendar.Gregorian' later added to keep the calendar aligned to the solar year.  Years use astronomical numbering, so year 1 is AD 1, year 0 is 1 BC, year -1 is 2 BC
-- and so on; dates run from the calendar's introduction on 1.January.45 BC (year -44) onward, with no upper bound.  Dates share the same absolute timeline as every other calendar, so in the modern era
-- a Julian date reads 13 days behind the same instant's Gregorian date.  The Julian calendar is not merely historical \- the Eastern Orthodox churches still use it liturgically, so it stays useful for
-- both past dates and current and future feast-day calculations.
--
-- == Proleptic every-fourth-year rule
--
-- This implementation applies the clean every-fourth-year rule uniformly from 45 BC onward.  It deliberately does /not/ reproduce the calendar's messy early history, in which the priests who
-- administered it inserted a leap day every three years by mistake (the "triennial error") until Augustus suspended leap years to realign it, the regular rule only settling in by around AD 8.  We omit
-- that for two reasons:
--
-- * The exact sequence of long years during 45 BC \- AD 7 is genuinely disputed: Scaliger, Ideler and Bennett each reconstruct it differently, so an "accurate" version would just bake one contested
--   interpretation in as fact.
--
-- * By long-standing convention historians and astronomers already cite ancient dates in the /proleptic/ Julian calendar (the clean rule), precisely because the real sequence is uncertain.  For example
--   "15.March.44 BC" for Caesar's assassination is a proleptic Julian date; modelling the errors would make this library disagree with the way such dates are normally written.
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

-- | Julian dates are valid from the calendar's introduction, 1.January.45 BC (astronomical year -44), onward; earlier
--   dates are rejected \- the calendar did not exist and this implementation does not extend it backwards.  There is no
--   upper bound beyond the 'Int32' day representation.  This tuple is also the floor the shared lens\/constructor
--   helpers clamp to.
firstJulDayTuple :: (Integral a, Integral b, Integral c) => (a, b, c)
firstJulDayTuple = (-44, 0, 1)        -- NOTE: 1.Jan.45 BC

invalidDayThresh :: Integral a => a
invalidDayThresh = fromIntegral $ pred day0
  where
    (y, m, d) = firstJulDayTuple :: (Year, Int, DayOfMonth)
    day0 = yearMonthDayToDays y (toEnum m) d

epochDayOfWeek :: DayOfWeek Julian
epochDayOfWeek = Tuesday

-- | Julian works in its own frame: internal flat day 0 is 1.Mar.2000 in the Julian calendar.  Only the 'Instant'
--   bridge crosses to the universal timeline (day 0 = 1.Mar.2000 Gregorian), where Julian's epoch sits 13 days later
--   (the Julian\/Gregorian divergence), so 'toUnadjustedInstant' adds this offset and 'fromAdjustedInstant' subtracts
--   it.  Because it is the gap between two fixed absolute days, the offset is constant for all of time.
julianEpochOffset :: Num a => a
julianEpochOffset = 13

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
  fromAdjustedInstant (Instant days secs nsecs) = CalendarDateTime (julianFromDays (days - julianEpochOffset)) (LocalTime secs nsecs)
  toUnadjustedInstant (CalendarDateTime jd (LocalTime secs nsecs)) = Instant (julianToDays jd + julianEpochOffset) secs nsecs

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
    days = yearDays + commonMonthDayOffsets !! m' + d - 1

daysToYearMonthDay :: Int32 -> (Int32, Word8, Word8)
daysToYearMonthDay days = (fromIntegral y, fromIntegral m'', fromIntegral d')
  where
    (fourYears, (remaining, isLeapDay)) = flip divMod daysPerFourYears >>> (* 4) *** id &&& borders daysPerFourYears $ days
    (oneYears, yearDays) = remaining `divMod` daysPerStandardYear
    -- NOTE: the sentinel 'daysPerStandardYear' lets February (yearDays >= the last real offset) be found; without it
    -- 'findIndex' returns Nothing and 'fromJust' crashes for any late-February date.
    m = pred . fromJust . findIndex (\mo -> yearDays < mo) $ commonMonthDayOffsets ++ [daysPerStandardYear]
    (m', startDate) = if m >= 10 then (m - 10, 2001) else (m + 2, 2000)
    d = yearDays - commonMonthDayOffsets !! m + 1
    (m'', d') = if isLeapDay then (1, 29) else (m', d)
    y = startDate + fourYears + oneYears