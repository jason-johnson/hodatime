{-# LANGUAGE TypeFamilies #-}

module Data.HodaTime.CalendarDateTime.Internal
(
   DayNth(..)
  ,Year
  ,WeekNumber
  ,DayOfMonth
  ,CalendarDate(..)
  ,CalendarDateTime(..)
  ,IsCalendar(..)
  ,HasDate(..)
  ,LocalTime(..)
  ,IsCalendarDateTime(..)
  ,at
)
where

import Data.HodaTime.Instant.Internal (Instant)
import Data.Int (Int32)
import Data.Word (Word8, Word32)

-- CalendarDate

-- | Used by several smart constructors to chose a day relative to the start or end of the month.
data DayNth =
    FourthToLast
  | ThirdToLast
  | SecondToLast
  | Last
  | First
  | Second
  | Third
  | Fourth
  | Fifth
  deriving (Eq, Show, Enum)

type Year = Int
type DayOfMonth = Int
type WeekNumber = Int

-- TODO: We may want to add a "cycle" field to the calendarDate that counts 400 year cyles.  This would allow days to be non-negative
--       and it would mean that one table can translate all possible days since they repeat each cycle

-- | Represents a specific date within its calendar system, with no reference to any time zone or time of day.
-- Note: We keep the date in 2 formats, redundantly.  We depend on lazy evaluation to only produce the portion that is actually used
data CalendarDate calendar = CalendarDate { cdDays :: Int32, cdDay :: Word8, cdMonth :: Word8, cdYear :: Word32 }
  deriving (Eq, Show, Ord)  -- TODO: Get rid of Show and define the other instances to only use cdDays

-- NOTE: This is a test form of the calendar date that only stores the cycle.  Everything else will be pulled from the date cache table, as required
--data CalendarDate o calendar = CalendarDate { cdDays :: Int32, cdCycle :: Word8, ldOptions :: o }
--  deriving (Eq, Show, Ord)

class IsCalendar cal where
  type Date cal
  data DayOfWeek cal
  data Month cal
  day' :: Functor f => (DayOfMonth -> f DayOfMonth) -> CalendarDate cal -> f (CalendarDate cal)
  month' :: CalendarDate cal -> Month cal
  monthl' :: Functor f => (Int -> f Int) -> CalendarDate cal -> f (CalendarDate cal)
  year' :: Functor f => (Year -> f Year) -> CalendarDate cal -> f (CalendarDate cal)
  dayOfWeek' :: CalendarDate cal -> DayOfWeek cal
  next' :: Int -> DayOfWeek cal -> CalendarDate cal -> CalendarDate cal
  previous' :: Int -> DayOfWeek cal -> CalendarDate cal -> CalendarDate cal

class HasDate d where
  type DoW d
  type MoY d
  -- | Lens for the day component of a 'HasDate'.  Please note that days are not clamped: if you add e.g. 400 days then the month and year will roll
  day :: Functor f => (DayOfMonth -> f DayOfMonth) -> d -> f d
  -- | Accessor for the Month component of a 'HasDate'.
  month :: d -> MoY d
  -- | Lens for interacting with the month component of a 'HasDate'.  Please note that we convert the month to an Int so meaningful math can be done on it.  Also
  --   please note that the day will be unaffected except in the case of "end of month" days which may clamp.  Note that this clamping will only occur as a final step,
  --   so that
  --
  --   >>> modify monthl (+ 2) <$> Gregorian.calendarDate 31 January 2000
  --   Just (CalendarDate 31 March 2000)
  --
  --   and not 29th of March as would happen with some libraries.
  monthl :: Functor f => (Int -> f Int) -> d -> f d
  -- | Lens for the year component of a 'HasDate'.  Please note that the rest of the date is left as is, with two exceptions: Feb 29 will clamp to 28 in a non-leapyear
  --   and if the new year is earlier than the earliest supported year it will clamp back to that year
  year :: Functor f => (Year -> f Year) -> d -> f d
  -- | Accessor for the Day of the week enum of a 'HasDate', for example:
  --
  -- >>> dayOfWeek . fromJust $ Gregorian.calendarDate 31 January 2000
  -- Monday
  dayOfWeek :: d -> DoW d
  -- | Returns a 'HasDate' shifted to the nth next Day of Week from the current 'HasDate', for example:
  --
  -- >>> next 1 Monday . fromJust $ Gregorian.calendarDate 31 January 2000
  -- CalendarDate 7 February 2000
  next :: Int -> DoW d -> d -> d
  -- | Returns a 'HasDate' shifted to the nth previous Day of Week from the current 'HasDate', for example:
  --
  -- >>> previous 1 Monday . fromJust $ Gregorian.calendarDate 31 January 2000
  -- CalendarDate 24 January 2000
  previous :: Int -> DoW d -> d -> d

instance (IsCalendar cal) => HasDate (CalendarDate cal) where
  type DoW (CalendarDate cal) = DayOfWeek cal
  type MoY (CalendarDate cal) = Month cal
  day = day'
  month = month'
  monthl = monthl'
  year = year'
  dayOfWeek = dayOfWeek'
  next = next'
  previous = previous'

-- LocalTime

-- | Represents a specific time of day with no reference to any calendar, date or time zone.
data LocalTime = LocalTime { ltSecs :: Word32, ltNsecs :: Word32 }
  deriving (Eq, Ord, Show)    -- TODO: Remove Show

-- CalendarDateTime

-- | Represents a specific date and time within its calendar system.  NOTE: a CalendarDateTime does
--   *not* represent a specific time on the global time line because e.g. "10.March.2006 4pm" is a different instant
--   in most time zones.  Convert it to a ZonedDateTime first if you wish to convert to an instant (or use a convenience
--   function).
data CalendarDateTime calendar = CalendarDateTime (CalendarDate calendar) LocalTime
  deriving (Eq, Show, Ord)

instance (IsCalendar cal) => HasDate (CalendarDateTime cal) where
  type DoW (CalendarDateTime cal) = DayOfWeek cal
  type MoY (CalendarDateTime cal) = Month cal
  day f (CalendarDateTime cd lt) = flip CalendarDateTime lt <$> day f cd
  month (CalendarDateTime cd _) = month cd
  monthl f (CalendarDateTime cd lt) = flip CalendarDateTime lt <$> monthl f cd
  year f (CalendarDateTime cd lt) = flip CalendarDateTime lt <$> year f cd
  dayOfWeek (CalendarDateTime cd _) = dayOfWeek cd
  next i dow (CalendarDateTime cd lt) = CalendarDateTime (next i dow cd) lt
  previous i dow (CalendarDateTime cd lt) = CalendarDateTime (previous i dow cd) lt

-- | Private class used to allow conversions to and from CalendarDateTime for a given calendar.  If you see this in the documentation, consider it a bug
class IsCalendarDateTime cal where
  -- | Convert an Instant which has already been converted to the correct time for the Calendar and TimeZone into CalendarDateTime
  fromAdjustedInstant :: Instant -> CalendarDateTime cal
  -- | Convert a CalendarDateTime directly to an Instant.  Needed because different calendars use different epochs.  If this ever changes we can revisit this
  toUnadjustedInstant :: CalendarDateTime cal -> Instant

-- constructors

-- | Returns a 'CalendarDateTime' of the 'CalendarDate' at the given 'LocalTime'
at :: CalendarDate cal -> LocalTime -> CalendarDateTime cal
at date time = CalendarDateTime date time