module Data.HodaTime.Calendar.Gregorian
(
  -- * Constructors
   calendarDate
  ,fromNthDay
  ,fromNthDay'
  ,fromWeekDate
  -- * Types
  ,Month(..)
  ,DayOfWeek(..)
  ,Gregorian
)
where

import Data.HodaTime.Calendar.Gregorian.Internal hiding (fromWeekDate)
import Data.HodaTime.CalendarDateTime.Internal (CalendarDate(..), DayNth, DayOfMonth, Year, WeekNumber)
import qualified Data.HodaTime.Calendar.Gregorian.Internal as GI
import Control.Monad (guard)

-- Constructors

-- | Smart constuctor for Gregorian calendar date.
calendarDate :: DayOfMonth -> Month Gregorian -> Year -> Maybe (CalendarDate Gregorian)
calendarDate d m y = do
  guard $ d > 0 && d <= maxDaysInMonth m y
  let days = fromIntegral $ yearMonthDayToDays y m d
  guard $ days > invalidDayThresh
  return $ CalendarDate days (fromIntegral d) (fromIntegral . fromEnum $ m) (fromIntegral y)

-- | Smart constuctor for Gregorian calendar date based on relative month day.
fromNthDay :: DayNth -> DayOfWeek Gregorian -> Month Gregorian -> Year -> Maybe (CalendarDate Gregorian)
fromNthDay nth dow m y = do
  guard $ adjustment < fromIntegral mdim           -- NOTE: we have to use < not <= because we're adding to first of the month or subtracting from the end of the month
  guard $ days > invalidDayThresh
  return $ CalendarDate (fromIntegral days) (fromIntegral d) (fromIntegral . fromEnum $ m) (fromIntegral y)
  where
    mdim = maxDaysInMonth m y
    somDays = yearMonthDayToDays y m 1
    eomDays = yearMonthDayToDays y m mdim
    startDow = dayOfWeekFromDays days'
    targetDow = fromEnum dow
    adjustment = 7 * multiple + adjust startDow targetDow
    (days', multiple, adjust, d, days) = frontOrBack (fromEnum nth)
    frontOrBack nth'
      | nth' < 5  = (somDays, nth', weekdayDistance, adjustment + 1, somDays + adjustment)
      | otherwise = (eomDays, nth' - 5, flip weekdayDistance, mdim - adjustment, eomDays - adjustment)

fromNthDay' :: DayNth -> DayOfWeek Gregorian -> Month Gregorian -> Year -> Maybe (CalendarDate Gregorian)
fromNthDay' nth dow m y = do
  guard $ d > 0 && d <= mdim
  guard $ days > invalidDayThresh
  return $ CalendarDate (fromIntegral days) (fromIntegral d) (fromIntegral . fromEnum $ m) (fromIntegral y)
  where
    nth' = fromEnum nth
    nth'' = if nth' < 5 then nth' else nth' - 5
    mdim = maxDaysInMonth m y
    d = nthDayToDayOfMonth nth'' (fromEnum dow) m y
    days = yearMonthDayToDays y m d

-- | Smart constuctor for Gregorian calendar date based on week date.  Note that this method assumes weeks start on Sunday and the first week of the year is the one
--   which has at least one day in the new year.  For ISO compliant behavior use this constructor from the ISO module
fromWeekDate :: WeekNumber -> DayOfWeek Gregorian -> Year -> Maybe (CalendarDate Gregorian)
fromWeekDate = GI.fromWeekDate 1 Sunday

-- help functions

weekdayDistance :: (Ord a, Num a) => a -> a -> a
weekdayDistance s e = e' - s
  where
    e' = if e >= s then e else e + 7