{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Calendar.Islamic
-- Copyright   :  (C) 2017 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- This is the module for 'CalendarDate' and 'CalendarDateTime' in the 'Islamic' (Hijri) calendar, a purely lunar calendar of twelve months.  The odd-numbered months ('Muharram', 'RabiAlAwwal', … ) have
-- 30 days and the even-numbered months ('Safar', 'RabiAlThani', … ) have 29, except that the final month ('DhulHijjah') gains a thirtieth day in a leap year.  A common year is therefore 354 days and a
-- leap year 355 — roughly eleven days shorter than a solar year, so Islamic dates drift steadily backwards through the seasons.  Year 1 begins on 18.Jul.622 CE (proleptic Gregorian), the year of the
-- Hijra; dates share the same absolute timeline as every other calendar.
--
-- == Which Islamic calendar this is, and the choices we made
--
-- There is no single \"Islamic calendar\": the religiously authoritative one is /observational/ (each month begins on the naked-eye sighting of the new crescent), which is inherently non-algorithmic and
-- varies by location, so it cannot be computed.  What software can compute is either the /tabular/ (arithmetic) calendar or a tabulated astronomical calendar such as Umm al-Qura.  This module implements
-- the __tabular arithmetic__ calendar.  A tabular calendar leaves two parameters open — the /leap-year pattern/ and the /epoch/ — and we treat them differently:
--
--     * __Leap-year pattern: selectable, defaulting to \"Base16\" (type II).__  A leap pattern says which 11 of every 30 years carry the extra day.  Four patterns are in common use, and the calendar is
--       parameterised over them at the /type level/ (see below), so a date always records which pattern built it and the type system refuses to mix incompatible ones.  The default, __Base16__ — leap years
--       2, 5, 7, 10, 13, 16, 18, 21, 24, 26 and 29 of each cycle — is the pattern used by the .NET BCL @HijriCalendar@ and NodaTime's @IslamicBcl@, so it is the most interoperable choice and the one we
--       cross-check against.
--
--     * __Epoch: fixed to astronomical (\"Thursday\") — 18.Jul.622 CE (proleptic Gregorian), Julian day 1948439.__  The alternative \"civil\" (\"Friday\") epoch is exactly one day later.  Unlike the leap
--       pattern, the epoch does not change the calendar's internal structure (month lengths, leap years, arithmetic); it only shifts how Islamic dates line up with the absolute timeline — i.e. their
--       'Data.HodaTime.Instant.Instant', their Gregorian correspondence and their day-of-week — by that one day.  Because it is a one-day alignment convention rather than a structurally different calendar,
--       we fix it (to the astronomical epoch, matching the .NET BCL and NodaTime) rather than expose it.  Were it ever wanted it would become a second type parameter in exactly the same way as the leap
--       pattern, a non-breaking change (today's @Islamic l@ would become a synonym for @Islamic l Astronomical@).
--
-- Being purely arithmetic, the calendar is exact by definition (there is no astronomical approximation, unlike the astronomical Persian calendar) and total for every year, so — like the Coptic calendar —
-- it is only floored at year 1 (the Hijra) and has no upper bound.  Note that the tabular calendar can differ from an actual crescent sighting, and from the Umm al-Qura calendar, by a day or two; if you
-- need to match observation you must use sighting data, which is outside the scope of an arithmetic calendar.
--
-- == Selecting a leap pattern
--
-- The calendar type carries the leap pattern as a type parameter of kind 'LeapPattern': @'Islamic' l@.  This is why ordinary, non-configurable calendars such as 'Data.HodaTime.Calendar.Gregorian.Gregorian'
-- are unaffected — only a calendar that actually has a choice to record gains a parameter, and it is always fully applied (e.g. @'CalendarDateTime' ('Islamic' 'Base15')@).  Because the parameter is phantom,
-- the month and weekday constructors ('Muharram', 'Sunday', … ) are shared across every variant, but two dates built with different patterns have different types and cannot be combined or compared.
--
-- For convenience each pattern has a type synonym — 'IslamicBcl' (the Base16 default), 'IslamicBase15', 'IslamicIndian' and 'IslamicHabashAlHasib' — and the constructors come in two forms:
--
--     * 'calendarDate', 'fromNthDay' and 'fromWeekDate' build the default 'IslamicBcl' calendar and need no annotation.
--
--     * @calendarDate'@, @fromNthDay'@ and @fromWeekDate'@ are polymorphic in the pattern; choose one with a type annotation or @TypeApplications@, e.g. @calendarDate' \@Base15 d m y@ or
--       @calendarDate' d m y :: Maybe ('CalendarDate' 'IslamicBase15')@.
----------------------------------------------------------------------------
module Data.HodaTime.Calendar.Islamic
(
  -- * Constructors (default 'IslamicBcl' calendar)
   calendarDate
  ,fromNthDay
  ,fromWeekDate
  -- * Constructors (choose the leap pattern)
  ,calendarDate'
  ,fromNthDay'
  ,fromWeekDate'
  -- * Types
  ,Month(..)
  ,DayOfWeek(..)
  ,Islamic
  ,LeapPattern(..)
  ,KnownLeap
  -- * Named calendars (leap-pattern type synonyms)
  ,IslamicBcl
  ,IslamicBase15
  ,IslamicBase16
  ,IslamicIndian
  ,IslamicHabashAlHasib
)
where

import Data.HodaTime.CalendarDateTime.Internal (IsCalendar(..), IsCalendarDateTime(..), CalendarDate, DayNth, DayOfMonth, Year, WeekNumber, CalendarDateTime(..), LocalTime(..), Date)
import Data.HodaTime.Instant.Internal (Instant(..))
import Data.HodaTime.Calendar.Internal (mkCommonDayLens, mkCommonMonthLens, mkYearLens, mkFromNthDay, mkFromWeekDate, moveByDow, dayOfWeekFromDays)
import Data.Bits ((.&.), shiftL, testBit, popCount)
import Data.Int (Int32)
import Data.Word (Word8)
import Control.Monad (guard)

-- constants

monthsPerYear :: Int
monthsPerYear = 12

daysPerNonLeapYear :: Int
daysPerNonLeapYear = 354

daysPerLeapYear :: Int
daysPerLeapYear = 355

-- | Days in one 30-year leap cycle: 19 common years of 354 days plus 11 leap years of 355.
daysPerCycle :: Int
daysPerCycle = 19 * daysPerNonLeapYear + 11 * daysPerLeapYear   -- 10631

-- | The four tabular leap-year patterns in common use, used as the (kind-'LeapPattern') type parameter of 'Islamic'.
--   Each names which 11 of the 30 cycle years carry the extra day; 'Base16' is the .NET BCL \/ NodaTime default (see
--   the module header).
data LeapPattern = Base15 | Base16 | Indian | HabashAlHasib

-- | Reflects a 'LeapPattern' type down to its leap-year bit set: bit @n@ is set when year @n@ of the 30-year cycle
--   (0-based, so @year \`mod\` 30@) is a leap year.  Use @TypeApplications@ to read it, e.g. @'leapPatternBits' \@Base16@.
class KnownLeap (l :: LeapPattern) where
  leapPatternBits :: Int

instance KnownLeap 'Base15        where leapPatternBits = 623158436     -- leap years 2,5,7,10,13,15,18,21,24,26,29
instance KnownLeap 'Base16        where leapPatternBits = 623191204     -- leap years 2,5,7,10,13,16,18,21,24,26,29 (.NET BCL / NodaTime)
instance KnownLeap 'Indian        where leapPatternBits = 690562340     -- leap years 2,5,8,10,13,16,19,21,24,27,29
instance KnownLeap 'HabashAlHasib where leapPatternBits = 153692453     -- leap years 2,5,8,11,13,16,19,21,24,27,30

-- | Days elapsed before each 0-based month, ignoring the leap day (which only affects the final month's own length).
--   Odd-numbered (1-based) months have 30 days and even-numbered months 29.
islamicMonthDayOffsets :: [Int]
islamicMonthDayOffsets = [0, 30, 59, 89, 118, 148, 177, 207, 236, 266, 295, 325]

-- | Universal flat day (day 0 = 1.Mar.2000 Gregorian) of 1.Muharram.1 — the astronomical (\"Thursday\") epoch, Julian
--   day 1948439 = 18.Jul.622 CE (proleptic Gregorian).  Islamic dates are stored directly on the universal timeline, so
--   the 'Instant' bridge is the identity and this constant appears only inside the day\/date conversions.
islamicEpoch :: Int
islamicEpoch = -503166

firstIslDayTuple :: (Integral a, Integral b, Integral c) => (a, b, c)
firstIslDayTuple = (1, 0, 1)        -- NOTE: 1.Muharram.1

-- | 1.Muharram.1 sits exactly on the epoch (year 1 has no preceding days and Muharram is the first month), so a day is
--   \"before the calendar starts\" when it is earlier than the epoch.  This threshold is independent of the leap pattern.
invalidDayThresh :: Integral a => a
invalidDayThresh = fromIntegral (pred islamicEpoch)

-- | Islamic dates are stored directly on the universal timeline (day 0 = 1.Mar.2000 Gregorian = Wednesday), so the
--   'Instant' bridge is the identity and the epoch weekday is that of the shared day 0.  It is shared by every leap
--   pattern (the parameter @l@ is phantom).
epochDayOfWeek :: DayOfWeek (Islamic l)
epochDayOfWeek = Wednesday

-- types

-- | The Islamic (Hijri) calendar, parameterised by its leap-year pattern (see 'LeapPattern' and the module header).
data Islamic (l :: LeapPattern)

-- | The Base15 tabular calendar.
type IslamicBase15        = Islamic 'Base15
-- | The Base16 tabular calendar (same as 'IslamicBcl').
type IslamicBase16        = Islamic 'Base16
-- | The Indian tabular calendar.
type IslamicIndian        = Islamic 'Indian
-- | The Habash al-Hasib tabular calendar.
type IslamicHabashAlHasib = Islamic 'HabashAlHasib
-- | The default Islamic calendar: the Base16 leap pattern, matching the .NET BCL @HijriCalendar@ and NodaTime's @IslamicBcl@.
type IslamicBcl           = Islamic 'Base16

instance KnownLeap l => IsCalendar (Islamic l) where
  data Date (Islamic l) = IslamicDate {-# UNPACK #-} !Int32 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Int32
    deriving (Eq, Show, Ord)

  data DayOfWeek (Islamic l) = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

  data Month (Islamic l) = Muharram | Safar | RabiAlAwwal | RabiAlThani | JumadaAlAwwal | JumadaAlThani | Rajab | Shaban | Ramadan | Shawwal | DhulQadah | DhulHijjah
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

  fromDays = islamicFromDays (leapPatternBits @l)
  toDays = islamicToDays
  toYmd = islamicToYmd

  day' = let b = leapPatternBits @l in mkCommonDayLens invalidDayThresh (yearMonthDayToDays b) (islamicFromDays b) islamicToYmd
  {-# INLINE day' #-}

  month' (IslamicDate _ _ m _) = toEnum . fromIntegral $ m

  monthl' = let b = leapPatternBits @l in mkCommonMonthLens monthsPerYear firstIslDayTuple (maxDaysInMonth b) (yearMonthDayToDays b) islamicToYmd (islamicFromDays b)
  {-# INLINE monthl' #-}

  year' = let b = leapPatternBits @l in mkYearLens firstIslDayTuple (maxDaysInMonth b) (yearMonthDayToDays b) islamicToYmd (islamicFromDays b)
  {-# INLINE year' #-}

  dayOfWeek' (IslamicDate days _ _ _) = toEnum . dayOfWeekFromDays epochDayOfWeek . fromIntegral $ days

  next' n dow (IslamicDate days _ _ _) = moveByDow (islamicFromDays (leapPatternBits @l)) epochDayOfWeek n dow (-) (+) (>) (fromIntegral days)

  previous' n dow (IslamicDate days _ _ _) = moveByDow (islamicFromDays (leapPatternBits @l)) epochDayOfWeek n dow subtract (-) (<) (fromIntegral days)  -- NOTE: subtract is (-) with the arguments flipped

instance KnownLeap l => IsCalendarDateTime (Islamic l) where
  fromAdjustedInstant (Instant days secs nsecs) = CalendarDateTime (islamicFromDays (leapPatternBits @l) days) (LocalTime secs nsecs)
  toUnadjustedInstant (CalendarDateTime isd (LocalTime secs nsecs)) = Instant (islamicToDays isd) secs nsecs

-- | Build the flat Islamic date (denormalized: keeps the day count plus the decoded day\/month\/year).
islamicFromDays :: Int -> Int32 -> Date (Islamic l)
islamicFromDays bits days = IslamicDate days d m y
  where (y, m, d) = daysToYearMonthDay bits days

islamicToDays :: Date (Islamic l) -> Int32
islamicToDays (IslamicDate days _ _ _) = days

islamicToYmd :: Date (Islamic l) -> (Int32, Word8, Word8)
islamicToYmd (IslamicDate _ d m y) = (y, m, d)

-- Constructors

-- | Smart constructor for the default 'IslamicBcl' calendar date.  Returns 'Nothing' if the day is out of range for the
--   month or the year is before the epoch (year 1).  Use @calendarDate'@ to pick a different leap pattern.
calendarDate :: DayOfMonth -> Month IslamicBcl -> Year -> Maybe (CalendarDate IslamicBcl)
calendarDate = calendarDate'

-- | Smart constructor for an 'Islamic' calendar date in any leap pattern (chosen by the result type).  Returns 'Nothing'
--   if the day is out of range for the month or the year is before the epoch (year 1).
calendarDate' :: forall l. KnownLeap l => DayOfMonth -> Month (Islamic l) -> Year -> Maybe (CalendarDate (Islamic l))
calendarDate' d m y = do
  guard $ d > 0 && d <= maxDaysInMonth bits m y
  let days = fromIntegral $ yearMonthDayToDays bits y m d
  guard $ days > invalidDayThresh
  return $ islamicFromDays bits days
  where bits = leapPatternBits @l

-- | Smart constructor for the default 'IslamicBcl' calendar date given as a day relative to a month (e.g. the third Monday of the month).  Returns 'Nothing' if the resulting date is invalid.
fromNthDay :: DayNth -> DayOfWeek IslamicBcl -> Month IslamicBcl -> Year -> Maybe (CalendarDate IslamicBcl)
fromNthDay = fromNthDay'

-- | As 'fromNthDay', but in any leap pattern (chosen by the result type).
fromNthDay' :: forall l. KnownLeap l => DayNth -> DayOfWeek (Islamic l) -> Month (Islamic l) -> Year -> Maybe (CalendarDate (Islamic l))
fromNthDay' = mkFromNthDay invalidDayThresh epochDayOfWeek (yearMonthDayToDays bits) (maxDaysInMonth bits) (islamicFromDays bits)
  where bits = leapPatternBits @l

-- | Smart constructor for the default 'IslamicBcl' calendar date given as a week date.  Note that this method assumes weeks start on Saturday (as in the Islamic calendar) and the first week of the year is
--   the one which has at least one day in the new year.
fromWeekDate :: WeekNumber -> DayOfWeek IslamicBcl -> Year -> Maybe (CalendarDate IslamicBcl)
fromWeekDate = fromWeekDate'

-- | As 'fromWeekDate', but in any leap pattern (chosen by the result type).
fromWeekDate' :: forall l. KnownLeap l => WeekNumber -> DayOfWeek (Islamic l) -> Year -> Maybe (CalendarDate (Islamic l))
fromWeekDate' = mkFromWeekDate invalidDayThresh epochDayOfWeek (yearMonthDayToDays bits) (islamicFromDays bits) 1 Saturday
  where bits = leapPatternBits @l

-- helper functions

-- | A year is a leap year when the bit at @year \`mod\` 30@ of the given leap-pattern bit set is set.
isLeapYear :: Int -> Year -> Bool
isLeapYear bits y = testBit bits (fromIntegral (y `mod` 30))

maxDaysInMonth :: Int -> Month (Islamic l) -> Year -> Int
maxDaysInMonth bits DhulHijjah y
  | isLeapYear bits y                      = 30
  | otherwise                              = 29
maxDaysInMonth _ m _
  | even (fromEnum m)                      = 30        -- odd (1-based) months: Muharram, RabiAlAwwal, …
  | otherwise                              = 29        -- even (1-based) months: Safar, RabiAlThani, …

-- | Number of leap years among the first @r@ years of a cycle (cycle positions 1 .. @r@, for @r@ in 0 .. 29).
leapsInFirst :: Int -> Int -> Int
leapsInFirst bits r = popCount (bits .&. ((1 `shiftL` (r + 1)) - 2))

-- | Universal flat day (day 0 = 1.Mar.2000 Gregorian) of the given Islamic date, for the given leap-pattern bit set.
yearMonthDayToDays :: Int -> Year -> Month (Islamic l) -> DayOfMonth -> Int
yearMonthDayToDays bits y m d = islamicEpoch + daysBeforeYear + islamicMonthDayOffsets !! fromEnum m + d - 1
  where
    (c, r) = (fromIntegral y - 1) `divMod` 30
    daysBeforeYear = c * daysPerCycle + r * daysPerNonLeapYear + leapsInFirst bits r

daysToYearMonthDay :: Int -> Int32 -> (Int32, Word8, Word8)
daysToYearMonthDay bits flatDays = (fromIntegral y, fromIntegral m, fromIntegral d)
  where
    n = fromIntegral flatDays - islamicEpoch                         -- days since 1.Muharram.1 (>= 0 for valid dates)
    (cycles, remCycle) = n `divMod` daysPerCycle                     -- 10631-day, 30-year cycle
    (yearInCycle, dayOfYear) = findYear 0 0
    y = cycles * 30 + yearInCycle + 1
    (m, d)
      | dayOfYear == daysPerNonLeapYear    = (11, 30)                -- 355th day: DhulHijjah 30 (leap years only)
      | otherwise                          = (dayOfYear * 2 `div` 59, (dayOfYear `mod` 59) `mod` 30 + 1)
    -- Walk the (at most 30) years of the cycle, subtracting each year's length until the remaining days fall inside one.
    findYear i acc
      | acc + diy > remCycle               = (i, remCycle - acc)
      | otherwise                          = findYear (i + 1) (acc + diy)
      where diy = if testBit bits ((i + 1) `mod` 30) then daysPerLeapYear else daysPerNonLeapYear