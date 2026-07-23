{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Calendar.Persian
-- Copyright   :  (C) 2017 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- This is the module for 'CalendarDate' and 'CalendarDateTime' in the 'Persian' (Solar Hijri) calendar, the official calendar of Iran and Afghanistan.  It is a solar calendar whose year begins at the
-- vernal equinox (Nowruz, around 20\/21 March): the first six months ('Farvardin' through 'Shahrivar') have 31 days, the next five ('Mehr' through 'Bahman') have 30, and the final month ('Esfand') has
-- 29, or 30 in a leap year.  Year 1 begins on 22.Mar.622 CE (proleptic Gregorian), the year of the Hijra; dates share the same absolute timeline as every other calendar.
--
-- == Leap years: the astronomical calendar
--
-- This is the /astronomical/ Solar Hijri calendar — the official civil calendar of Iran (equivalent to the .NET @PersianCalendar@ from 4.6 onwards and NodaTime's @PersianAstronomical@).  Rather than an
-- arithmetic cycle, a year is a leap year exactly when the astronomical rule places the following Nowruz 366 days later.  Nowruz is the day on which the March equinox occurs if the equinox is before true
-- noon at the reference meridian (52.5°E, Iran Standard Time), otherwise the next day.  The equinox and leap years are computed in "Data.HodaTime.Calendar.Persian.Astronomical"; the table is built lazily,
-- so programs that don't use the Persian calendar pay nothing for it.
--
-- == Supported range and why it is capped at 1500
--
-- The calendar is vouched for over Persian years 1 .. 1500 (≈ 622 .. 2121 CE); 'calendarDate' rejects years outside that range.  This is a deliberately narrower window than NodaTime's
-- @PersianAstronomical@, which spans years 1 .. 9377 (≈ 622 .. 9999 CE).  The difference is not an oversight but a consequence of /how/ each library determines Nowruz:
--
-- * NodaTime embeds a fixed lookup table of leap-year bits that was generated once from the .NET 4.6 BCL @PersianCalendar@.  Every year up to 9377 therefore has a /frozen, deterministic/ answer,
--   even far-future years for which the "astronomical" leap flag is really just whatever value the BCL happened to precompute.
--
-- * We instead compute each Nowruz on demand from first principles: the March equinox (Meeus, /Astronomical Algorithms/ ch. 27\/28), the equation of time (to reduce mean noon to apparent noon at
--   the Iranian reference meridian), and ΔT — the difference between Terrestrial Time and Universal Time caused by the slow, irregular change in the Earth's rotation.
--
-- ΔT is the limiting factor.  It can only be /measured/ for the past and /extrapolated/ for the future, and the standard Espenak–Meeus polynomials are only trustworthy through roughly 2150 CE; beyond
-- that the extrapolation error grows without bound and can shift the computed equinox — and hence a borderline Nowruz — by a whole day.  Capping at Persian year 1500 (≈ 2121 CE) keeps every result
-- inside the range where the astronomy is genuinely well constrained, so every leap year we report is defensible rather than speculative.  Over this range our results match NodaTime exactly, including
-- all of the documented years where the astronomical calendar diverges from the arithmetic one (e.g. Persian 1404 = 21.Mar.2025, where the arithmetic calendar gives the 20th).
--
-- If you need dates past 2121 CE, prefer the arithmetic (Birashk) Solar Hijri calendar, whose leap rule is exact by definition and carries no astronomical uncertainty.
----------------------------------------------------------------------------
module Data.HodaTime.Calendar.Persian
(
  -- * Constructors
   calendarDate
  ,fromNthDay
  ,fromWeekDate
  -- * Types
  ,Month(..)
  ,DayOfWeek(..)
  ,Persian
)
where

import Data.HodaTime.CalendarDateTime.Internal (IsCalendar(..), IsCalendarDateTime(..), CalendarDate, DayNth, DayOfMonth, Year, WeekNumber, CalendarDateTime(..), LocalTime(..), Date)
import Data.HodaTime.Instant.Internal (Instant(..))
import Data.HodaTime.Calendar.Internal (mkCommonDayLens, mkCommonMonthLens, mkYearLens, mkFromNthDay, mkFromWeekDate, moveByDow, dayOfWeekFromDays)
import Data.HodaTime.Calendar.Persian.Astronomical (newYearDay, minPersianYear, maxPersianYear)
import Data.Int (Int32)
import Data.Word (Word8)
import Control.Monad (guard)

-- constants

monthsPerYear :: Int
monthsPerYear = 12

-- | Days elapsed before each 0-based month.  Months 1-6 have 31 days, months 7-11 have 30, 'Esfand' has 29 (30 in a leap year).
persianMonthDayOffsets :: [Int]
persianMonthDayOffsets = [0, 31, 62, 93, 124, 155, 186, 216, 246, 276, 306, 336]

firstPerDayTuple :: (Integral a, Integral b, Integral c) => (a, b, c)
firstPerDayTuple = (1, 0, 1)        -- NOTE: 1.Farvardin.1

invalidDayThresh :: Integral a => a
invalidDayThresh = fromIntegral $ pred day0
  where
    (y, m, d) = firstPerDayTuple :: (Year, Int, DayOfMonth)
    day0 = yearMonthDayToDays y (toEnum m) d

-- | Persian dates are stored directly on the universal timeline (day 0 = 1.Mar.2000 Gregorian = Wednesday), so the
--   'Instant' bridge is the identity and the epoch weekday is that of the shared day 0.
epochDayOfWeek :: DayOfWeek Persian
epochDayOfWeek = Wednesday

-- types

data Persian

instance IsCalendar Persian where
  data Date Persian = PersianDate {-# UNPACK #-} !Int32 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Int32
    deriving (Eq, Show, Ord)

  data DayOfWeek Persian = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

  data Month Persian = Farvardin | Ordibehesht | Khordad | Tir | Mordad | Shahrivar | Mehr | Aban | Azar | Dey | Bahman | Esfand
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

  fromDays = persianFromDays
  toDays = persianToDays
  toYmd = persianToYmd

  day' = mkCommonDayLens invalidDayThresh yearMonthDayToDays persianFromDays persianToYmd
  {-# INLINE day' #-}

  month' (PersianDate _ _ m _) = toEnum . fromIntegral $ m

  monthl' = mkCommonMonthLens monthsPerYear firstPerDayTuple maxDaysInMonth yearMonthDayToDays persianToYmd persianFromDays
  {-# INLINE monthl' #-}

  year' = mkYearLens firstPerDayTuple maxDaysInMonth yearMonthDayToDays persianToYmd persianFromDays
  {-# INLINE year' #-}

  dayOfWeek' (PersianDate days _ _ _) = toEnum . dayOfWeekFromDays epochDayOfWeek . fromIntegral $ days

  next' n dow (PersianDate days _ _ _) = moveByDow persianFromDays epochDayOfWeek n dow (-) (+) (>) (fromIntegral days)

  previous' n dow (PersianDate days _ _ _) = moveByDow persianFromDays epochDayOfWeek n dow subtract (-) (<) (fromIntegral days)  -- NOTE: subtract is (-) with the arguments flipped

instance IsCalendarDateTime Persian where
  fromAdjustedInstant (Instant days secs nsecs) = CalendarDateTime (persianFromDays days) (LocalTime secs nsecs)
  toUnadjustedInstant (CalendarDateTime pd (LocalTime secs nsecs)) = Instant (persianToDays pd) secs nsecs

-- | Build the flat Persian date (denormalized: keeps the day count plus the decoded day\/month\/year).
persianFromDays :: Int32 -> Date Persian
persianFromDays days = PersianDate days d m y
  where (y, m, d) = daysToYearMonthDay days

persianToDays :: Date Persian -> Int32
persianToDays (PersianDate days _ _ _) = days

persianToYmd :: Date Persian -> (Int32, Word8, Word8)
persianToYmd (PersianDate _ d m y) = (y, m, d)

-- Constructors

-- | Smart constructor for a 'Persian' calendar date.  Returns 'Nothing' if the day is out of range for the month or the
--   year is outside the supported range (1 .. 1500).
calendarDate :: DayOfMonth -> Month Persian -> Year -> Maybe (CalendarDate Persian)
calendarDate d m y = do
  guard $ y >= fromIntegral minPersianYear && y <= fromIntegral maxPersianYear
  guard $ d > 0 && d <= maxDaysInMonth m y
  return $ persianFromDays (fromIntegral $ yearMonthDayToDays y m d)

-- | Smart constructor for a 'Persian' calendar date given as a day relative to a month (e.g. the third Monday of the month).  Returns 'Nothing' if the resulting date is invalid.
fromNthDay :: DayNth -> DayOfWeek Persian -> Month Persian -> Year -> Maybe (CalendarDate Persian)
fromNthDay = mkFromNthDay invalidDayThresh epochDayOfWeek yearMonthDayToDays maxDaysInMonth persianFromDays

-- | Smart constructor for a 'Persian' calendar date given as a week date.  Note that this method assumes weeks start on Saturday (as in the Persian calendar) and the first week of the year is the
--   one which has at least one day in the new year.
fromWeekDate :: WeekNumber -> DayOfWeek Persian -> Year -> Maybe (CalendarDate Persian)
fromWeekDate = mkFromWeekDate invalidDayThresh epochDayOfWeek yearMonthDayToDays persianFromDays 1 Saturday

-- helper functions

-- | A year is a leap year exactly when the astronomical rule places the next Nowruz 366 days later.
isLeapYear :: Year -> Bool
isLeapYear y = newYearDay (fromIntegral y + 1) - newYearDay (fromIntegral y) == 366

maxDaysInMonth :: Month Persian -> Year -> Int
maxDaysInMonth Esfand y
  | isLeapYear y                           = 30
  | otherwise                              = 29
maxDaysInMonth m _
  | fromEnum m < 6                         = 31        -- Farvardin .. Shahrivar
  | otherwise                              = 30        -- Mehr .. Bahman

-- | Universal flat day (day 0 = 1.Mar.2000 Gregorian) of the given Persian date.
yearMonthDayToDays :: Year -> Month Persian -> DayOfMonth -> Int
yearMonthDayToDays y m d = newYearDay (fromIntegral y) + persianMonthDayOffsets !! fromEnum m + d - 1

daysToYearMonthDay :: Int32 -> (Int32, Word8, Word8)
daysToYearMonthDay flatDays = (fromIntegral y, fromIntegral m, fromIntegral d)
  where
    day = fromIntegral flatDays :: Int                               -- universal flat day
    yEst = minPersianYear + (day - newYearDay minPersianYear) `div` 365
    y = adjust yEst
    adjust yy
      | newYearDay yy > day        = adjust (yy - 1)
      | newYearDay (yy + 1) <= day = adjust (yy + 1)
      | otherwise                  = yy
    dayOfYear = day - newYearDay y
    (m, d)
      | dayOfYear == 365                   = (11, 30)                -- Esfand 30 (leap years only)
      | dayOfYear < 6 * 31                 = let (mm, dd) = dayOfYear `divMod` 31 in (mm, dd + 1)
      | otherwise                          = let (mm, dd) = (dayOfYear - 6 * 31) `divMod` 30 in (mm + 6, dd + 1)