{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.HodaTime.CalendarDateTime.Internal
(
   DayNth(..)
  ,Year
  ,WeekNumber
  ,DayOfMonth
  ,CalendarDate
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
import Control.DeepSeq (NFData(..))
import Data.Hashable (Hashable(..))

-- $setup
-- >>> import Data.Maybe (fromJust)
-- >>> import qualified Data.HodaTime.Calendar.Gregorian as Gregorian
-- >>> import Data.HodaTime.Calendar.Gregorian (Month(..), DayOfWeek(..))

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

-- | A calendar date in the calendar system @cal@.  This is a public synonym for the per-calendar representation
--   'Date': each calendar defines its own @data instance Date cal@ (see 'IsCalendar'), so unrelated calendars
--   (e.g. Gregorian and Hebrew) need share nothing in how a date is stored.
type CalendarDate cal = Date cal

class IsCalendar cal where
  -- | The per-calendar date representation.  Each calendar picks whatever packing is most natural/efficient for it.
  data Date cal
  data DayOfWeek cal
  data Month cal
  -- | Build a date from a flat, epoch-relative day count (the calendar's own epoch).
  fromDays :: Int32 -> Date cal
  -- | Extract the flat, epoch-relative day count from a date.
  toDays :: Date cal -> Int32
  -- | Decode a date to @(year, zero-based month, day-of-month)@.  The year is signed so calendars can represent
  --   pre-epoch (e.g. BC) years without wraparound.
  toYmd :: Date cal -> (Int32, Word8, Word8)
  -- | The calendar's display name (e.g. @"Gregorian"@), used by 'Show' to render a date as the
  --   smart-constructor call that builds it.
  calendarName :: Date cal -> String
  day' :: Date cal -> DayOfMonth
  setDay' :: DayOfMonth -> Date cal -> Date cal
  month' :: Date cal -> Month cal
  monthl' :: Date cal -> Int
  setMonthl' :: Int -> Date cal -> Date cal
  year' :: Date cal -> Year
  setYear' :: Year -> Date cal -> Date cal
  dayOfWeek' :: Date cal -> DayOfWeek cal
  next' :: Int -> DayOfWeek cal -> Date cal -> Date cal
  previous' :: Int -> DayOfWeek cal -> Date cal -> Date cal

class HasDate d where
  type DoW d
  type MoY d
  -- | Day-of-month component.
  day :: d -> DayOfMonth
  setDay :: DayOfMonth -> d -> d
  -- | Accessor for the Month component of a 'HasDate'.
  month :: d -> MoY d
  monthl :: d -> Int
  setMonthl :: Int -> d -> d
  -- | Year component.
  year :: d -> Year
  setYear :: Year -> d -> d
  -- | Accessor for the Day of the week enum of a 'HasDate', for example:
  --
  -- >>> dayOfWeek . fromJust $ Gregorian.calendarDate 31 January 2000
  -- Monday
  dayOfWeek :: d -> DoW d
  -- | Returns a 'HasDate' shifted to the nth next Day of Week from the current 'HasDate', for example:
  --
  -- >>> next 1 Monday . fromJust $ Gregorian.calendarDate 31 January 2000
  -- fromJust (Gregorian.calendarDate 7 February 2000)
  next :: Int -> DoW d -> d -> d
  -- | Returns a 'HasDate' shifted to the nth previous Day of Week from the current 'HasDate', for example:
  --
  -- >>> previous 1 Monday . fromJust $ Gregorian.calendarDate 31 January 2000
  -- fromJust (Gregorian.calendarDate 24 January 2000)
  previous :: Int -> DoW d -> d -> d
  -- | Access the year, month and day-of-month components together in a single call, returned as a
  --   @(year, month, day)@ tuple.
  --
  --   This is purely an access optimization for code that needs more than one date component at once.  Reading the
  --   components individually with 'year', 'month' and 'day' is perfectly correct, but for a packed representation
  --   (such as the Gregorian 'Date', which stores a cycle\/century\/day-in-century triple) each of those accessors
  --   independently decodes the stored value, so asking for all three separately decodes it three times.
  --   'yearMonthDay' decodes once and hands back every component, which is noticeably cheaper on hot paths (for
  --   example date formatting).  For representations that already store the components separately (such as the Julian
  --   'Date') there is nothing to decode and this is simply the three field reads, so it is never slower than the
  --   individual accessors and callers can use it unconditionally.
  yearMonthDay :: d -> (Year, MoY d, DayOfMonth)
  yearMonthDay d = (year d, month d, day d)

instance (IsCalendar cal) => HasDate (Date cal) where
  type DoW (Date cal) = DayOfWeek cal
  type MoY (Date cal) = Month cal
  day = day'
  setDay = setDay'
  month = month'
  monthl = monthl'
  setMonthl = setMonthl'
  year = year'
  setYear = setYear'
  dayOfWeek = dayOfWeek'
  next = next'
  previous = previous'

-- | Renders the (partial) smart-constructor call that produces a date, e.g. @Gregorian.calendarDate 31 March 2000@,
--   without the surrounding 'fromJust'.  Shared by the 'Show' instances for 'Date' and 'CalendarDateTime'.
showsDateCon :: (IsCalendar cal, Show (Month cal)) => Date cal -> ShowS
showsDateCon date =
    showString (calendarName date) . showString ".calendarDate "
  . showsPrec 11 (fromIntegral dom :: Int) . showChar ' '
  . showsPrec 11 (month' date) . showChar ' '
  . showsPrec 11 (fromIntegral yr :: Int)
  where (yr, _m, dom) = toYmd date

-- | Renders a date as the (honest) smart-constructor call that produces it, e.g.
--   @fromJust (Gregorian.calendarDate 31 March 2000)@.  This is a debug rendering, not code to paste back
--   verbatim (the calendar qualifier depends on how you imported it, and 'calendarDate' returns 'Maybe') \- it
--   is meant to tell you exactly what the value is.
instance (IsCalendar cal, Show (Month cal)) => Show (Date cal) where
  showsPrec p date = showParen (p > 10) $ showString "fromJust (" . showsDateCon date . showChar ')'

-- LocalTime

-- | Represents a specific time of day with no reference to any calendar, date or time zone.
data LocalTime = LocalTime { ltSecs :: Word32, ltNsecs :: Word32 }
  deriving (Eq, Ord)

-- | Renders the (partial) smart-constructor call that produces a 'LocalTime', e.g. @localTime 4 30 0 0@, without
--   the surrounding 'fromJust'.  Shared by the 'Show' instances for 'LocalTime' and 'CalendarDateTime'.
showsLocalTimeCon :: LocalTime -> ShowS
showsLocalTimeCon (LocalTime secs nsecs) =
    showString "localTime "
  . showsPrec 11 h . showChar ' ' . showsPrec 11 m . showChar ' '
  . showsPrec 11 s . showChar ' ' . showsPrec 11 (fromIntegral nsecs :: Int)
  where
    (h, r) = (fromIntegral secs :: Int) `divMod` 3600
    (m, s) = r `divMod` 60

instance Show LocalTime where
  showsPrec p lt = showParen (p > 10) $ showString "fromJust (" . showsLocalTimeCon lt . showChar ')'

instance NFData LocalTime where
  rnf (LocalTime secs nsecs) = rnf secs `seq` rnf nsecs

instance Hashable LocalTime where
  hashWithSalt s (LocalTime secs nsecs) = s `hashWithSalt` secs `hashWithSalt` nsecs

-- CalendarDateTime

-- | Represents a specific date and time within its calendar system.  NOTE: a CalendarDateTime does
--   *not* represent a specific time on the global time line because e.g. "10.March.2006 4pm" is a different instant
--   in most time zones.  Convert it to a ZonedDateTime first if you wish to convert to an instant (or use a convenience
--   function).
data CalendarDateTime calendar = CalendarDateTime (Date calendar) LocalTime

deriving instance Eq (Date cal) => Eq (CalendarDateTime cal)
deriving instance Ord (Date cal) => Ord (CalendarDateTime cal)

-- | Renders a 'CalendarDateTime' as the applicative construction that produces it, e.g.
--   @fromJust (at \<$\> Gregorian.calendarDate 31 March 2000 \<*\> localTime 4 30 0 0)@.  A single 'fromJust'
--   wraps the whole thing because that is how one actually builds the value from the (partial) smart constructors.
instance (IsCalendar cal, Show (Month cal)) => Show (CalendarDateTime cal) where
  showsPrec p (CalendarDateTime d lt) = showParen (p > 10) $
      showString "fromJust (at <$> " . showsDateCon d . showString " <*> " . showsLocalTimeCon lt . showChar ')'

instance NFData (Date cal) => NFData (CalendarDateTime cal) where
  rnf (CalendarDateTime d lt) = rnf d `seq` rnf lt

instance Hashable (Date cal) => Hashable (CalendarDateTime cal) where
  hashWithSalt s (CalendarDateTime d lt) = s `hashWithSalt` d `hashWithSalt` lt

instance (IsCalendar cal) => HasDate (CalendarDateTime cal) where
  type DoW (CalendarDateTime cal) = DayOfWeek cal
  type MoY (CalendarDateTime cal) = Month cal
  day (CalendarDateTime cd _) = day cd
  setDay value (CalendarDateTime cd lt) = CalendarDateTime (setDay value cd) lt
  month (CalendarDateTime cd _) = month cd
  monthl (CalendarDateTime cd _) = monthl cd
  setMonthl value (CalendarDateTime cd lt) = CalendarDateTime (setMonthl value cd) lt
  year (CalendarDateTime cd _) = year cd
  setYear value (CalendarDateTime cd lt) = CalendarDateTime (setYear value cd) lt
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
at :: Date cal -> LocalTime -> CalendarDateTime cal
at = CalendarDateTime