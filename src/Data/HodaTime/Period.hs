{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.HodaTime.Period
-- Description : Calendar-relative amounts of time.
--
-- A 'Period' is indexed by the type it can be applied to. Unit constructors
-- constrain that target, so combining date and time units with '<>' infers a
-- target that supports both sets of fields.
--
-- For example, a mixed period can be applied to a 'CalendarDateTime', while
-- either half can also be used independently with a date or time:
--
-- @
-- applyPeriod (months 5 <> hours 2) calendarDateTime
-- applyPeriod (months 5) calendarDate
-- applyPeriod (hours 2) localTime
-- @
--
-- A reusable top-level binding needs either a target annotation or
-- @NoMonomorphismRestriction@. With the latter, GHC generalizes
-- @months 5 <> hours 2@ to a period requiring both 'HasDate' and
-- 'HasLocalTime'.
module Data.HodaTime.Period
(
   Period
  ,years
  ,months
  ,weeks
  ,days
  ,hours
  ,minutes
  ,seconds
  ,nanoseconds
  ,negatePeriod
  ,scalePeriod
  ,ApplyPeriod(..)
)
where

import Data.Functor.Identity (Identity(..))
import Data.HodaTime.CalendarDateTime.Internal
  (CalendarDateTime(..), Date, HasDate(..), IsCalendar(..), LocalTime(..))
import Data.HodaTime.LocalTime.Internal (HasLocalTime)

-- | A calendar-relative amount applicable to @target@.
--
-- The constructor is hidden so the constraints introduced by the unit
-- constructors cannot be bypassed.
data Period target = Period
  { periodYears :: !Int
  , periodMonths :: !Int
  , periodWeeks :: !Int
  , periodDays :: !Int
  , periodHours :: !Int
  , periodMinutes :: !Int
  , periodSeconds :: !Int
  , periodNanoseconds :: !Int
  }
  deriving (Eq, Show)

type role Period nominal

instance Semigroup (Period target) where
  Period y1 mo1 w1 d1 h1 mi1 s1 ns1 <> Period y2 mo2 w2 d2 h2 mi2 s2 ns2 =
    Period (y1 + y2) (mo1 + mo2) (w1 + w2) (d1 + d2)
      (h1 + h2) (mi1 + mi2) (s1 + s2) (ns1 + ns2)

instance Monoid (Period target) where
  mempty = Period 0 0 0 0 0 0 0 0

-- | Construct a period measured in calendar years.
years, months, weeks, days :: HasDate target => Int -> Period target
years value = mempty { periodYears = value }
-- | Construct a period measured in calendar months.
months value = mempty { periodMonths = value }
-- | Construct a period measured in seven-day calendar weeks.
weeks value = mempty { periodWeeks = value }
-- | Construct a period measured in calendar days.
days value = mempty { periodDays = value }

-- | Construct a period measured in hours.
hours, minutes, seconds, nanoseconds :: HasLocalTime target => Int -> Period target
hours value = mempty { periodHours = value }
-- | Construct a period measured in minutes.
minutes value = mempty { periodMinutes = value }
-- | Construct a period measured in seconds.
seconds value = mempty { periodSeconds = value }
-- | Construct a period measured in nanoseconds.
nanoseconds value = mempty { periodNanoseconds = value }

-- | Negate every component of a period.
negatePeriod :: Period target -> Period target
negatePeriod = scalePeriod (-1)

-- | Multiply every component of a period by an integer.
scalePeriod :: Int -> Period target -> Period target
scalePeriod factor (Period y mo w d h mi s ns) =
  Period (factor * y) (factor * mo) (factor * w) (factor * d)
    (factor * h) (factor * mi) (factor * s) (factor * ns)

-- | Types to which periods can be applied.
class ApplyPeriod target where
  applyPeriod :: Period target -> target -> target

instance ApplyPeriod LocalTime where
  applyPeriod period = snd . applyTimePeriod period

instance IsCalendar cal => ApplyPeriod (Date cal) where
  applyPeriod = applyDatePeriod

instance IsCalendar cal => ApplyPeriod (CalendarDateTime cal) where
  applyPeriod period (CalendarDateTime date time) =
    CalendarDateTime (shiftDateByDays carry (applyDatePeriod period date)) time'
    where
      (carry, time') = applyTimePeriod period time

-- Noda Time applies period fields from largest to smallest. Keeping year and
-- month as separate operations preserves that behavior when either clamps an
-- end-of-month date.
applyDatePeriod :: HasDate target => Period periodTarget -> target -> target
applyDatePeriod period =
    shiftDateByDays (7 * periodWeeks period + periodDays period)
  . modifyDate monthl (periodMonths period)
  . modifyDate year (periodYears period)

shiftDateByDays :: HasDate target => Int -> target -> target
shiftDateByDays = modifyDate day

modifyDate
  :: ((Int -> Identity Int) -> target -> Identity target)
  -> Int
  -> target
  -> target
modifyDate lens amount = runIdentity . lens (Identity . (+ amount))

applyTimePeriod :: Period target -> LocalTime -> (Int, LocalTime)
applyTimePeriod period (LocalTime currentSeconds currentNanoseconds) =
  (fromInteger carry, LocalTime (fromInteger secondsOfDay) (fromInteger nanos))
  where
    nanosPerSecond = 1000000000 :: Integer
    nanosPerDay = 86400 * nanosPerSecond
    current =
      (toInteger currentSeconds * nanosPerSecond) + toInteger currentNanoseconds
    delta =
      (((toInteger (periodHours period) * 60
        + toInteger (periodMinutes period)) * 60
        + toInteger (periodSeconds period)) * nanosPerSecond)
        + toInteger (periodNanoseconds period)
    (carry, withinDay) = (current + delta) `divMod` nanosPerDay
    (secondsOfDay, nanos) = withinDay `divMod` nanosPerSecond