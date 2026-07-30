{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Calendar.Hebrew
-- Copyright   :  (C) 2017 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- This is the module for 'CalendarDate' and 'CalendarDateTime' in the 'Hebrew' (Jewish) calendar, a lunisolar calendar that keeps its lunar months in step with the solar year by inserting a thirteenth
-- month ('AdarI') seven times in each nineteen-year Metonic cycle (in years 3, 6, 8, 11, 14, 17 and 19).  A common year has twelve months and a leap year thirteen; two of the months ('Cheshvan' and
-- 'Kislev') can independently gain or lose a day so that the year comes out to one of six permitted lengths (353\/354\/355 days common, 383\/384\/385 leap).  The year number changes at 1 'Tishri' (Rosh
-- Hashanah), and dates share the same absolute timeline as every other calendar.
--
-- == Month numbering: civil vs. scriptural
--
-- There are two traditional ways to /number/ the Hebrew months, and — as with the Islamic leap patterns — the calendar is parameterised over the choice at the /type level/ so that a date always records which
-- convention it uses and the type system refuses to mix the two.  The choice does not change the underlying calendar at all (the months, their lengths and the day a given date falls on are identical); it only
-- changes which ordinal number 'fromEnum' \/ 'toEnum' assign to each month:
--
--     * __Civil__ (the default) counts from 'Tishri', the month of Rosh Hashanah at which the year number turns over.  This is the numbering used by the .NET @HebrewCalendar@ and is the natural one for most
--       software: Tishri = 1, Cheshvan = 2, … , with the leap month 'AdarI' falling in the middle of the sequence in a leap year.
--
--     * __Scriptural__ (biblical\/ecclesiastical) counts from 'Nisan', the \"first month\" of the Exodus.  Nisan = 1, Iyar = 2, … , Tishri = 7, and the leap month 'AdarI' is numbered last (after 'Shevat'),
--       keeping the numbers of the other twelve months fixed between common and leap years.
--
-- The month /constructors/ are shared across both conventions (the parameter is phantom for storage), so 'Nisan' is the same month either way — only its 'Enum' number differs.  Each convention has a type
-- synonym, 'HebrewCivil' and 'HebrewScriptural', and 'HebrewCivil' is the default.
--
-- == The two Adars
--
-- In a common year there is a single month 'Adar'.  A leap year inserts an extra month, 'AdarI' (Adar Rishon), /before/ it; the original 'Adar' then plays the role of \"Adar II\" (Adar Sheni) and still
-- carries Purim.  So 'Adar' is present every year and 'AdarI' exists only in leap years — attempting to build a date in 'AdarI' in a common year yields 'Nothing', exactly as an out-of-range day or year does.
----------------------------------------------------------------------------
module Data.HodaTime.Calendar.Hebrew
(
  -- * Constructors (default civil calendar)
   calendarDate
  ,fromNthDay
  ,fromWeekDate
  -- * Constructors (choose the numbering)
  ,calendarDate'
  ,fromNthDay'
  ,fromWeekDate'
  -- * Types
  ,Hebrew
  ,MonthNumbering(..)
  ,Month(..)
  ,DayOfWeek(..)
  ,KnownNumbering
  -- * Named calendars (month-numbering type synonyms)
  ,HebrewCivil
  ,HebrewScriptural
)
where

import Data.HodaTime.CalendarDateTime.Internal (IsCalendar(..), IsCalendarDateTime(..), CalendarDate, DayNth, DayOfMonth, Year, WeekNumber, CalendarDateTime(..), LocalTime(..), Date)
import Control.DeepSeq (NFData(..))
import Data.Hashable (Hashable(..))
import Data.HodaTime.Instant.Internal (Instant(..))
import Data.HodaTime.Calendar.Internal (mkFromNthDay, moveByDow, dayOfWeekFromDays)
import Data.Int (Int32)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Control.Monad (guard)

-- types

-- | The two conventions for numbering the Hebrew months, used as the (kind-'MonthNumbering') type parameter of 'Hebrew'
--   (see the module header).
data MonthNumbering = Civil | Scriptural

-- | The Hebrew (Jewish) calendar, parameterised by its month-numbering convention (see 'MonthNumbering').
data Hebrew (n :: MonthNumbering)

-- | The Hebrew calendar numbered from 'Tishri' (the .NET @HebrewCalendar@ convention).  This is the default.
type HebrewCivil      = Hebrew 'Civil
-- | The Hebrew calendar numbered from 'Nisan' (the biblical \/ ecclesiastical convention).
type HebrewScriptural = Hebrew 'Scriptural

-- | Reflects a 'MonthNumbering' down to the calendar-order index at which it begins counting months: civil starts at
--   'Tishri' (0) and scriptural at 'Nisan' (7).  Read it via @TypeApplications@, e.g. @'numberingStart' \@'Civil'@.
class KnownNumbering (n :: MonthNumbering) where
  numberingStart :: Int

instance KnownNumbering 'Civil      where numberingStart = 0
instance KnownNumbering 'Scriptural where numberingStart = 7   -- 'Nisan' is calendar-order index 7

-- | The Hebrew calendar.  All of the field lenses and conversions run in numbering-independent calendar order; the
--   numbering only selects how 'Month' values are numbered by 'Enum'.
instance KnownNumbering n => IsCalendar (Hebrew n) where
  -- | Denormalized: the flat, epoch-relative day count plus the decoded day, calendar-order month index and year.
  data Date (Hebrew n) = HebrewDate {-# UNPACK #-} !Int32 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Int32
    deriving (Eq, Ord)

  -- | The Hebrew months, listed in calendar order from 'Tishri'.  'AdarI' (the leap month) sits between 'Shevat' and
  --   'Adar'; it exists only in leap years.  The constructors are shared by both numbering conventions — only their
  --   'Enum' numbers differ.
  data Month (Hebrew n) =
      Tishri | Cheshvan | Kislev | Tevet | Shevat | AdarI | Adar | Nisan | Iyar | Sivan | Tammuz | Av | Elul
    deriving (Show, Read, Eq, Ord, Bounded)

  data DayOfWeek (Hebrew n) = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

  fromDays = hebrewFromDays
  toDays = hebrewToDays
  toYmd = hebrewToYmd
  calendarName _ = "Hebrew"

  day' = hebrewDayLens
  {-# INLINE day' #-}

  month' (HebrewDate _ _ ci _) = monthAt (fromIntegral ci)

  monthl' = hebrewMonthLens (numberingStart @n)
  {-# INLINE monthl' #-}

  year' = hebrewYearLens
  {-# INLINE year' #-}

  dayOfWeek' (HebrewDate days _ _ _) = toEnum . dayOfWeekFromDays epochDayOfWeek . fromIntegral $ days

  next' i dow (HebrewDate days _ _ _) = moveByDow hebrewFromDays epochDayOfWeek i dow (-) (+) (>) (fromIntegral days)

  previous' i dow (HebrewDate days _ _ _) = moveByDow hebrewFromDays epochDayOfWeek i dow subtract (-) (<) (fromIntegral days)  -- NOTE: subtract is (-) with the arguments flipped

-- | The 'Month' 'Enum' is the one place the numbering shows: it is calendar order rotated so counting begins at the
--   numbering's start month ('numberingStart').
instance KnownNumbering n => Enum (Month (Hebrew n)) where
  fromEnum m = (calendarIndex m - numberingStart @n) `mod` monthCount
  toEnum i
    | i >= 0 && i < monthCount = monthAt ((i + numberingStart @n) `mod` monthCount)
    | otherwise               = error "Data.HodaTime.Calendar.Hebrew: toEnum: month out of range 0..12"

instance NFData (Date (Hebrew n)) where
  rnf (HebrewDate days d m y) = rnf days `seq` rnf d `seq` rnf m `seq` rnf y

instance Hashable (Date (Hebrew n)) where
  hashWithSalt s (HebrewDate days d m y) = s `hashWithSalt` days `hashWithSalt` d `hashWithSalt` m `hashWithSalt` y

instance NFData (Month (Hebrew n)) where
  rnf m = m `seq` ()

instance KnownNumbering n => Hashable (Month (Hebrew n)) where
  hashWithSalt s = hashWithSalt s . fromEnum

instance NFData (DayOfWeek (Hebrew n)) where
  rnf d = d `seq` ()

instance Hashable (DayOfWeek (Hebrew n)) where
  hashWithSalt s = hashWithSalt s . fromEnum

instance IsCalendarDateTime (Hebrew n) where
  fromAdjustedInstant (Instant days secs nsecs) = CalendarDateTime (hebrewFromDays days) (LocalTime secs nsecs)
  toUnadjustedInstant (CalendarDateTime hd (LocalTime secs nsecs)) = Instant (hebrewToDays hd) secs nsecs

-- constructors

-- | Smart constructor for a date in the default civil-numbered Hebrew calendar.  Returns 'Nothing' if the day is out of
--   range for the month (including 'AdarI' in a common year) or the year is before 1.  Use @calendarDate'@ to select the
--   scriptural numbering.
calendarDate :: DayOfMonth -> Month HebrewCivil -> Year -> Maybe (CalendarDate HebrewCivil)
calendarDate = calendarDate'

-- | Smart constructor for a 'Hebrew' date in either numbering (chosen by the result type).  Returns 'Nothing' if the
--   day is out of range for the month (including 'AdarI' in a common year) or the year is before 1.
calendarDate' :: DayOfMonth -> Month (Hebrew n) -> Year -> Maybe (CalendarDate (Hebrew n))
calendarDate' d m y = do
  guard $ y >= 1
  guard $ d > 0 && d <= maxDaysInMonth m y
  let days = hebrewYearMonthDayToDays y (calendarIndex m) d
  guard $ days > invalidDayThresh
  return $ hebrewFromDays (fromIntegral days)

-- | Smart constructor for the default civil calendar given as a day relative to a month (e.g. the third Monday).
--   Returns 'Nothing' if the resulting date is invalid.
fromNthDay :: DayNth -> DayOfWeek HebrewCivil -> Month HebrewCivil -> Year -> Maybe (CalendarDate HebrewCivil)
fromNthDay = fromNthDay'

-- | As 'fromNthDay', but in either numbering (chosen by the result type).
fromNthDay' :: KnownNumbering n => DayNth -> DayOfWeek (Hebrew n) -> Month (Hebrew n) -> Year -> Maybe (CalendarDate (Hebrew n))
fromNthDay' = mkFromNthDay invalidDayThresh epochDayOfWeek ymd mdim hebrewFromDays
  where
    ymd y m d = hebrewYearMonthDayToDays y (calendarIndex m) d
    mdim m y = maxDaysInMonth m y

-- | Smart constructor for the default civil calendar given as a week date.  Weeks are taken to start on 'Sunday' (as in
--   the Hebrew week) and week 1 is the one containing 1.'Tishri'.
fromWeekDate :: WeekNumber -> DayOfWeek HebrewCivil -> Year -> Maybe (CalendarDate HebrewCivil)
fromWeekDate = fromWeekDate'

-- | As 'fromWeekDate', but in either numbering (chosen by the result type).
fromWeekDate' :: WeekNumber -> DayOfWeek (Hebrew n) -> Year -> Maybe (CalendarDate (Hebrew n))
fromWeekDate' weekNum dow y = do
  guard $ days > invalidDayThresh
  return $ hebrewFromDays (fromIntegral days)
    where
      startOfYear = hebrewYearMonthDayToDays y 0 1                -- 1.Tishri (calendar-order index 0)
      startDoW = dayOfWeekFromDays epochDayOfWeek startOfYear
      weekStartDays = startOfYear - startDoW                      -- the Sunday on or before 1.Tishri
      days = weekStartDays + pred weekNum * 7 + fromEnum dow

-- helper functions

-- | The Hebrew months in calendar order, starting at 'Tishri': the single source of truth for month ordering.  Both
--   numbering conventions derive from it (civil counts from the head; scriptural is this cycle rotated to begin at
--   'Nisan'), so their 'Enum' numbers cannot drift out of step.
calendarMonths :: [Month (Hebrew n)]
calendarMonths = [Tishri, Cheshvan, Kislev, Tevet, Shevat, AdarI, Adar, Nisan, Iyar, Sivan, Tammuz, Av, Elul]

-- | The number of month constructors (13: the twelve common-year months plus the leap month 'AdarI').
monthCount :: Int
monthCount = length calendarMonths

-- | A month's 0-based position in calendar order (from 'Tishri'), independent of the numbering convention.  This is
--   the internal, storage-facing ordinal (and the civil 'Enum' number).
calendarIndex :: Month (Hebrew n) -> Int
calendarIndex m = fromJust (elemIndex m calendarMonths)

-- | Inverse of 'calendarIndex', checked against the valid 0..12 range.
monthAt :: Int -> Month (Hebrew n)
monthAt i
  | i >= 0 && i < monthCount = calendarMonths !! i
  | otherwise               = error "Data.HodaTime.Calendar.Hebrew: month index out of range 0..12"

-- | Calendar-order index of the leap month 'AdarI'.
adarICalIndex :: Int
adarICalIndex = 5

-- | Calendar-order index of 'Adar' (\"Adar II\" in a leap year).
adarCalIndex :: Int
adarCalIndex = 6

-- | Hebrew dates are stored directly on the universal timeline (day 0 = 1.Mar.2000 Gregorian = Wednesday), so the
--   'Instant' bridge is the identity and the epoch weekday is that of the shared day 0.
epochDayOfWeek :: DayOfWeek (Hebrew n)
epochDayOfWeek = Wednesday

-- | The day before 1.Tishri.1: a day is \"before the calendar starts\" when it is on or before this.
invalidDayThresh :: Int
invalidDayThresh = pred (firstDayOfYear 1)

-- | Anchors the molad-based day count to the shared timeline: the universal flat day of 1.Tishri.1 (the Hebrew epoch,
--   Rata Die -1373427 = universal day -2103607) less @'elapsedDays' 1@ (which is 1).  So
--   @'firstDayOfYear' = 'hebrewRefDay' + 'elapsedDays'@.
hebrewRefDay :: Int
hebrewRefDay = -2103608

-- Molad and Rosh Hashanah

-- | A year is a leap year (thirteen months) in 7 of every 19 (years 3, 6, 8, 11, 14, 17 and 19 of the Metonic cycle).
isLeapYear :: Year -> Bool
isLeapYear y = (7 * y + 1) `mod` 19 < 7

-- | Number of months in a year (12 common, 13 leap).
monthsInYear :: Year -> Int
monthsInYear y = if isLeapYear y then 13 else 12

-- | Lunar months from the epoch molad to the molad of 'Tishri' of the given year.
monthsElapsed :: Year -> Int
monthsElapsed year = 235 * cycles + 12 * inCycle + (7 * inCycle + 1) `div` 19
  where (cycles, inCycle) = (year - 1) `divMod` 19

-- | Days from the epoch molad to 1.Tishri of the given year after the four postponement rules (dechiyot): the molad
--   rules (molad zaken, and the GaTaRaD \/ BeTUTaKPaT year-length fixes) and \"lo ADU rosh\" (Rosh Hashanah may not
--   fall on Sunday, Wednesday or Friday).  A part (chelek) is 1\/1080 of an hour, so a day is 25920 parts.
elapsedDays :: Year -> Int
elapsedDays year = aduAdjusted
  where
    m = monthsElapsed year
    partsElapsed = 204 + 793 * (m `mod` 1080)
    hoursElapsed = 5 + 12 * m + 793 * (m `div` 1080) + partsElapsed `div` 1080
    moladDay = 1 + 29 * m + hoursElapsed `div` 24
    moladParts = 1080 * (hoursElapsed `mod` 24) + partsElapsed `mod` 1080
    postponed
      | moladParts >= 19440                                                   = moladDay + 1   -- molad zaken (molad at or after 18h)
      | moladDay `mod` 7 == 2 && moladParts >= 9924  && not (isLeapYear year) = moladDay + 1   -- GaTaRaD (Tue, 9h204p, common year)
      | moladDay `mod` 7 == 1 && moladParts >= 16789 && isLeapYear (year - 1) = moladDay + 1   -- BeTUTaKPaT (Mon, 15h589p, year after leap)
      | otherwise                                                             = moladDay
    aduAdjusted = if postponed `mod` 7 `elem` [0, 3, 5] then postponed + 1 else postponed

-- | Universal flat day (day 0 = 1.Mar.2000 Gregorian) of 1.Tishri of the given year (Rosh Hashanah).
firstDayOfYear :: Year -> Int
firstDayOfYear year = hebrewRefDay + elapsedDays year

-- | Length of the given year in days (353\/354\/355 common, 383\/384\/385 leap).
daysInYear :: Year -> Int
daysInYear year = firstDayOfYear (year + 1) - firstDayOfYear year

-- | In a \"complete\" (shelemah) year 'Cheshvan' has 30 days instead of 29.
isCheshvanLong :: Year -> Bool
isCheshvanLong year = daysInYear year `mod` 10 == 5

-- | In a \"deficient\" (chaserah) year 'Kislev' has 29 days instead of 30.
isKislevShort :: Year -> Bool
isKislevShort year = daysInYear year `mod` 10 == 3

-- | Length of the month at the given calendar-order index in the given year (0 for 'AdarI' in a common year, so any
--   day in it is out of range).
monthLengthByIndex :: Year -> Int -> Int
monthLengthByIndex year ci = case ci of
  0  -> 30                                    -- Tishri
  1  -> if isCheshvanLong year then 30 else 29-- Cheshvan
  2  -> if isKislevShort  year then 29 else 30-- Kislev
  3  -> 29                                    -- Tevet
  4  -> 30                                    -- Shevat
  5  -> if isLeapYear year then 30 else 0     -- AdarI (leap years only)
  6  -> 29                                    -- Adar (Adar II in leap years)
  7  -> 30                                    -- Nisan
  8  -> 29                                    -- Iyar
  9  -> 30                                    -- Sivan
  10 -> 29                                    -- Tammuz
  11 -> 30                                    -- Av
  12 -> 29                                    -- Elul
  _  -> 0

-- | Maximum day for a month in a given year (0 for 'AdarI' in a common year, so such a date is always rejected).
maxDaysInMonth :: Month (Hebrew n) -> Year -> Int
maxDaysInMonth m year = monthLengthByIndex year (calendarIndex m)

-- | Days elapsed in the year before the first of the month at the given calendar-order index.
daysBeforeMonth :: Year -> Int -> Int
daysBeforeMonth year ci = sum [monthLengthByIndex year i | i <- [0 .. ci - 1]]

-- Date <-> day count

-- | Universal flat day (day 0 = 1.Mar.2000 Gregorian) of the given year, calendar-order month index and day.
hebrewYearMonthDayToDays :: Year -> Int -> DayOfMonth -> Int
hebrewYearMonthDayToDays year ci d = firstDayOfYear year + daysBeforeMonth year ci + d - 1

-- | Decode a universal flat day to @(year, calendar-order month index, day)@.
hebrewDaysToYmd :: Int32 -> (Int32, Word8, Word8)
hebrewDaysToYmd flatDays = (fromIntegral year, fromIntegral ci, fromIntegral d)
  where
    n = fromIntegral flatDays :: Int
    year = findYear (estimate n)
    (ci, d) = findMonth 0 (n - firstDayOfYear year)
    findMonth i remaining
      | remaining < len = (i, remaining + 1)                  -- a 0-length AdarI in a common year is skipped here
      | otherwise       = findMonth (i + 1) (remaining - len)
      where len = monthLengthByIndex year i
    -- a Hebrew year averages ~365.25 days, so dividing by 366 gives a safe under-estimate to search up from
    estimate d0 = max 1 $ (d0 - firstDayOfYear 1) `div` 366 + 1
    findYear y
      | firstDayOfYear y > n        = findYear (y - 1)
      | firstDayOfYear (y + 1) <= n = findYear (y + 1)
      | otherwise                   = y

-- | Build the flat Hebrew date (denormalized: keeps the day count plus the decoded day\/calendar-month\/year).
hebrewFromDays :: Int32 -> Date (Hebrew n)
hebrewFromDays flatDays = HebrewDate flatDays d ci y
  where (y, ci, d) = hebrewDaysToYmd flatDays

hebrewToDays :: Date (Hebrew n) -> Int32
hebrewToDays (HebrewDate flatDays _ _ _) = flatDays

hebrewToYmd :: Date (Hebrew n) -> (Int32, Word8, Word8)
hebrewToYmd (HebrewDate _ d ci y) = (y, ci, d)

-- lenses (Hebrew-specific: the shared helpers assume a fixed month count and that the stored month index equals the
-- numbering's Enum number, neither of which holds here, so these work in numbering-independent calendar-index space)

-- | Day-of-month lens: shifts the flat day directly, so overflowing the month rolls into the next (not clamped).
hebrewDayLens :: Functor f => (DayOfMonth -> f DayOfMonth) -> Date (Hebrew n) -> f (Date (Hebrew n))
hebrewDayLens f date = rebuild <$> f (fromIntegral d)
  where
    (y, ci, d) = hebrewToYmd date
    startOfMonth = pred $ hebrewYearMonthDayToDays (fromIntegral y) (fromIntegral ci) 1
    rebuild newDay = hebrewFromDays (fromIntegral days)
      where
        raw = startOfMonth + newDay
        days = if raw > invalidDayThresh then raw else invalidDayThresh + 1

-- | Month lens.  The exposed 'Int' is the month in the calendar's numbering; adding to it moves that many months in
--   calendar order (carrying the year at 'Tishri' and skipping the absent 'AdarI' in common years), clamping the day.
hebrewMonthLens :: Functor f => Int -> (Int -> f Int) -> Date (Hebrew n) -> f (Date (Hebrew n))
hebrewMonthLens start f date = rebuild <$> f current
  where
    (y, ci, d) = hebrewToYmd date
    yr = fromIntegral y
    cur = fromIntegral ci
    current = (cur - start) `mod` monthCount
    rebuild target = hebrewFromDays (fromIntegral days)
      where
        (y', ci') = addMonthsChrono yr cur (target - current)
        mdim = monthLengthByIndex (max 1 y') ci'
        d' = min (fromIntegral d) mdim
        raw = hebrewYearMonthDayToDays (max 1 y') ci' d'
        days = if raw > invalidDayThresh then raw else invalidDayThresh + 1

-- | Year lens: keeps the month and day (clamping the day, and moving 'AdarI' to 'Adar' when the target year is common).
hebrewYearLens :: Functor f => (Year -> f Year) -> Date (Hebrew n) -> f (Date (Hebrew n))
hebrewYearLens f date = rebuild <$> f (fromIntegral y)
  where
    (y, ci, d) = hebrewToYmd date
    rebuild newYear = hebrewFromDays (fromIntegral days)
      where
        y' = max 1 newYear
        ci' = if fromIntegral ci == adarICalIndex && not (isLeapYear y') then adarCalIndex else fromIntegral ci
        mdim = monthLengthByIndex y' ci'
        d' = min (fromIntegral d) mdim
        days = hebrewYearMonthDayToDays y' ci' d'

-- | Move a @(year, calendar-order month index)@ by a number of months in calendar order, carrying the year at 'Tishri'
--   and stepping over the absent 'AdarI' in common years.
addMonthsChrono :: Year -> Int -> Int -> (Year, Int)
addMonthsChrono year ci delta = go year (calIndexToPos year ci + delta)
  where
    go y p
      | p < 0               = go (y - 1) (p + monthsInYear (y - 1))
      | p >= monthsInYear y = go (y + 1) (p - monthsInYear y)
      | otherwise           = (y, posToCalIndex y p)

-- | Calendar-order index to sequential position within the year (common years have no 'AdarI', so 'Adar' onward shift down).
calIndexToPos :: Year -> Int -> Int
calIndexToPos year ci
  | isLeapYear year    = ci
  | ci < adarICalIndex = ci
  | otherwise          = ci - 1

-- | Inverse of 'calIndexToPos'.
posToCalIndex :: Year -> Int -> Int
posToCalIndex year p
  | isLeapYear year   = p
  | p < adarICalIndex = p
  | otherwise         = p + 1