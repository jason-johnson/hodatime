{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Calendar.Coptic
-- Copyright   :  (C) 2017 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- This is the module for 'CalendarDate' and 'CalendarDateTime' in the 'Coptic' calendar, the liturgical calendar of the Coptic Orthodox Church whose era of the Martyrs (Anno Martyrum) counts years
-- from AD 284.  The year is twelve months of 30 days followed by a short thirteenth month ('PiKogiEnavot', the epagomenal days) of five days, or six in a leap year.  Leap years follow the same simple
-- every-fourth-year rule as 'Data.HodaTime.Calendar.Julian' (a Coptic year is leap when @year \`mod\` 4 == 3@), with the extra day added at the end of the year.  Year 1 begins on 29.Aug.284 in the
-- Julian calendar; dates share the same absolute timeline as every other calendar.
----------------------------------------------------------------------------
module Data.HodaTime.Calendar.Coptic
(
  -- * Constructors
   calendarDate
  ,fromNthDay
  ,fromWeekDate
  -- * Types
  ,Month(..)
  ,DayOfWeek(..)
  ,Coptic
)
where

import Data.HodaTime.CalendarDateTime.Internal (IsCalendar(..), IsCalendarDateTime(..), CalendarDate, DayNth, DayOfMonth, Year, WeekNumber, CalendarDateTime(..), LocalTime(..), Date)
import Data.HodaTime.Instant.Internal (Instant(..))
import Data.HodaTime.Calendar.Internal (mkCommonDayLens, mkCommonMonthLens, mkYearLens, mkFromNthDay, mkFromWeekDate, moveByDow, dayOfWeekFromDays, daysPerStandardYear, daysPerFourYears)
import Data.Int (Int32)
import Data.Word (Word8, Word32)
import Control.Monad (guard)

-- constants

monthsPerYear :: Int
monthsPerYear = 13

daysPerMonth :: Int
daysPerMonth = 30

-- | The day-of-year (0-based) at which the thirteenth month (the epagomenal days) begins.
daysBeforeEpagomenae :: Int
daysBeforeEpagomenae = 12 * daysPerMonth      -- 360

-- | Flat day (shared absolute epoch, day 0 = 1.Mar.2000 Gregorian) of Coptic 1.Thout.1, i.e. 29.Aug.284 Julian.
copticEpoch :: Num a => a
copticEpoch = -626575

firstCopDayTuple :: (Integral a, Integral b, Integral c) => (a, b, c)
firstCopDayTuple = (1, 0, 1)        -- NOTE: 1.Thout.AM 1

invalidDayThresh :: Integral a => a
invalidDayThresh = fromIntegral $ pred day0
  where
    (y, m, d) = firstCopDayTuple :: (Year, Int, DayOfMonth)
    day0 = yearMonthDayToDays y (toEnum m) d

epochDayOfWeek :: DayOfWeek Coptic
epochDayOfWeek = Wednesday

-- types

data Coptic

instance IsCalendar Coptic where
  data Date Coptic = CopticDate {-# UNPACK #-} !Int32 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word32
    deriving (Eq, Show, Ord)

  data DayOfWeek Coptic = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

  data Month Coptic = Thout | Paopi | Hathor | Koiak | Tobi | Meshir | Paremhat | Paremoude | Pashons | Paoni | Epip | Mesori | PiKogiEnavot
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

  fromDays = copticFromDays
  toDays = copticToDays
  toYmd = copticToYmd

  day' = mkCommonDayLens invalidDayThresh yearMonthDayToDays copticFromDays copticToYmd
  {-# INLINE day' #-}

  month' (CopticDate _ _ m _) = toEnum . fromIntegral $ m

  monthl' = mkCommonMonthLens monthsPerYear firstCopDayTuple maxDaysInMonth yearMonthDayToDays copticToYmd copticFromDays
  {-# INLINE monthl' #-}

  year' = mkYearLens firstCopDayTuple maxDaysInMonth yearMonthDayToDays copticToYmd copticFromDays
  {-# INLINE year' #-}

  dayOfWeek' (CopticDate days _ _ _) = toEnum . dayOfWeekFromDays epochDayOfWeek . fromIntegral $ days

  next' n dow (CopticDate days _ _ _) = moveByDow copticFromDays epochDayOfWeek n dow (-) (+) (>) (fromIntegral days)

  previous' n dow (CopticDate days _ _ _) = moveByDow copticFromDays epochDayOfWeek n dow subtract (-) (<) (fromIntegral days)  -- NOTE: subtract is (-) with the arguments flipped

instance IsCalendarDateTime Coptic where
  fromAdjustedInstant (Instant days secs nsecs) = CalendarDateTime (copticFromDays days) (LocalTime secs nsecs)
  toUnadjustedInstant (CalendarDateTime cd (LocalTime secs nsecs)) = Instant (copticToDays cd) secs nsecs

-- | Build the flat Coptic date (denormalized: keeps the day count plus the decoded day\/month\/year).
copticFromDays :: Int32 -> Date Coptic
copticFromDays days = CopticDate days d m y
  where (y, m, d) = daysToYearMonthDay days

copticToDays :: Date Coptic -> Int32
copticToDays (CopticDate days _ _ _) = days

copticToYmd :: Date Coptic -> (Word32, Word8, Word8)
copticToYmd (CopticDate _ d m y) = (y, m, d)

-- Constructors

-- | Smart constructor for a 'Coptic' calendar date.
calendarDate :: DayOfMonth -> Month Coptic -> Year -> Maybe (CalendarDate Coptic)
calendarDate d m y = do
  guard $ d > 0 && d <= maxDaysInMonth m y
  let days = fromIntegral $ yearMonthDayToDays y m d
  guard $ days > invalidDayThresh
  return $ copticFromDays days

-- | Smart constructor for a 'Coptic' calendar date given as a day relative to a month (e.g. the third Monday of the month).  Returns 'Nothing' if the resulting date is invalid.
fromNthDay :: DayNth -> DayOfWeek Coptic -> Month Coptic -> Year -> Maybe (CalendarDate Coptic)
fromNthDay = mkFromNthDay invalidDayThresh epochDayOfWeek yearMonthDayToDays maxDaysInMonth copticFromDays

-- | Smart constructor for a 'Coptic' calendar date given as a week date.  Note that this method assumes weeks start on Sunday and the first week of the year is the one
--   which has at least one day in the new year.
fromWeekDate :: WeekNumber -> DayOfWeek Coptic -> Year -> Maybe (CalendarDate Coptic)
fromWeekDate = mkFromWeekDate invalidDayThresh epochDayOfWeek yearMonthDayToDays copticFromDays 1 Sunday

-- helper functions

maxDaysInMonth :: Month Coptic -> Year -> Int
maxDaysInMonth PiKogiEnavot y
  | isLeap                                 = 6
  | otherwise                              = 5
  where
    isLeap                                 = 3 == y `mod` 4
maxDaysInMonth _ _                         = daysPerMonth

yearMonthDayToDays :: Year -> Month Coptic -> DayOfMonth -> Int
yearMonthDayToDays y m d = copticEpoch + (y - 1) * daysPerStandardYear + y `div` 4 + fromEnum m * daysPerMonth + d - 1

daysToYearMonthDay :: Int32 -> (Word32, Word8, Word8)
daysToYearMonthDay flatDays = (fromIntegral y, fromIntegral m, fromIntegral d)
  where
    n = fromIntegral flatDays - copticEpoch                          -- days since 1.Thout.1 (>= 0 for valid dates)
    (fourYears, remaining) = n `divMod` daysPerFourYears             -- 1461-day cycle, leap year last in each block
    (yearInBlock, dayOfYear)
      | remaining < 365       = (0, remaining)
      | remaining < 730       = (1, remaining - 365)
      | remaining < 1096      = (2, remaining - 730)                 -- the leap year (366 days)
      | otherwise             = (3, remaining - 1096)
    y = 4 * fourYears + 1 + yearInBlock
    (m, d)
      | dayOfYear >= daysBeforeEpagomenae  = (12, dayOfYear - daysBeforeEpagomenae + 1)
      | otherwise                          = let (mm, dd) = dayOfYear `divMod` daysPerMonth in (mm, dd + 1)