-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Pattern.OffsetDateTime
-- Copyright   :  (C) 2017 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- Patterns for an 'Data.HodaTime.OffsetDateTime.OffsetDateTime' — a
-- 'Data.HodaTime.CalendarDateTime.CalendarDateTime' pinned by a fixed UTC 'Data.HodaTime.Offset.Offset' — rendered as
-- @yyyy-MM-ddTHH:mm:ss(+\/-)HH:mm@.  Build your own with @offsetDateTimePattern@ from a date-time pattern and an offset
-- pattern.
----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
module Data.HodaTime.Pattern.OffsetDateTime
(
  -- * Standard Patterns
   pOffsetDateTime
  -- * Custom Patterns
  --
  -- | Build an 'OffsetDateTime' pattern from a 'CalendarDateTime' pattern and an 'Offset' pattern.
  ,offsetDateTimePattern
)
where

import Data.HodaTime.Pattern.Internal (Pattern, pairP)
import Data.HodaTime.Pattern.CalendarDateTime (ps)
import Data.HodaTime.Pattern.Offset (pOffset)
import Data.HodaTime.OffsetDateTime (OffsetDateTime, offset, toCalendarDateTime, fromCalendarDateTimeWithOffset)
import Data.HodaTime.CalendarDateTime (CalendarDateTime)
import Data.HodaTime.CalendarDateTime.Internal (IsCalendar)
import Data.HodaTime.Offset (Offset)

-- | Join a Gregorian-or-other 'CalendarDateTime' pattern to an 'Offset' pattern, producing an 'OffsetDateTime'
--   pattern.  The date\/time text comes first, immediately followed by the offset text.
offsetDateTimePattern
  :: IsCalendar cal
  => Pattern (CalendarDateTime cal -> CalendarDateTime cal) (CalendarDateTime cal -> String) String
  -> Pattern (Offset -> Offset) (Offset -> String) String
  -> Pattern (OffsetDateTime cal -> OffsetDateTime cal) (OffsetDateTime cal -> String) String
offsetDateTimePattern = pairP toCalendarDateTime offset fromCalendarDateTimeWithOffset

-- | The ISO-8601 offset date/time pattern, @yyyy-MM-ddTHH:mm:ss±HH:mm@ (e.g. @2024-04-23T09:00:00+02:00@).
pOffsetDateTime :: IsCalendar cal => Pattern (OffsetDateTime cal -> OffsetDateTime cal) (OffsetDateTime cal -> String) String
pOffsetDateTime = offsetDateTimePattern ps pOffset
