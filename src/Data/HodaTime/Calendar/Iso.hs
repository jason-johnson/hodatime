-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Calendar.Iso
-- Copyright   :  (C) 2017 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- This is the module for 'CalendarDate' and 'CalendarDateTime' in the ISO 8601 calendar.  For every date the ISO calendar is identical to 'Data.HodaTime.Calendar.Gregorian' \- the same months, the
-- same leap year rule and the same 15 October 1582 cut-off \- and it reuses the Gregorian types directly.  The two differ only in how week dates are numbered: ISO weeks start on Monday and week 1 is
-- the first week containing at least four days of the new year (equivalently, the week holding that year's first Thursday).  Use this module's 'fromWeekDate' for ISO-8601 week numbering; the Gregorian
-- module provides the alternative Sunday-based, at-least-one-day rule.
----------------------------------------------------------------------------
module Data.HodaTime.Calendar.Iso
(
  fromWeekDate
)
where

import Data.HodaTime.Calendar.Gregorian.Internal hiding (fromWeekDate)
import qualified Data.HodaTime.Calendar.Gregorian.Internal as GI
import Data.HodaTime.CalendarDateTime.Internal (CalendarDate, Year, WeekNumber)

-- | Smart constructor for an ISO 8601 week date.  Note that this method assumes weeks start on Monday and the first week of the year is the one
--   which has at least four days in the new year.  See the Gregorian module for alternative behavior
fromWeekDate :: WeekNumber -> DayOfWeek Gregorian -> Year -> Maybe (CalendarDate Gregorian)
fromWeekDate = GI.fromWeekDate 4 Monday