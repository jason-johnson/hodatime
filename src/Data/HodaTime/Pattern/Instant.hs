{-# LANGUAGE FlexibleContexts #-}
module Data.HodaTime.Pattern.Instant
(
  -- * Standard Patterns
   pInstant
  ,pInstantNano
  -- * Custom Patterns
  --
  -- | Build an 'Instant' pattern from any Gregorian 'CalendarDateTime' pattern, treating the instant as UTC.
  ,instantPattern
)
where

import Data.HodaTime.Pattern.Internal (Pattern, dimapP, (<%), char)
import Data.HodaTime.Pattern.CalendarDateTime (ps, po)
import Data.HodaTime.Instant.Internal (Instant)
import Data.HodaTime.CalendarDateTime.Internal (CalendarDateTime, IsCalendarDateTime(..))
import Data.HodaTime.Calendar.Gregorian (Gregorian)

-- | Adapt a Gregorian 'CalendarDateTime' pattern into an 'Instant' pattern.  The instant is treated as UTC: it is
--   projected to its unadjusted Gregorian 'CalendarDateTime' for formatting, and the parsed 'CalendarDateTime' is
--   taken back to an 'Instant' without any offset.
instantPattern
  :: Pattern (CalendarDateTime Gregorian -> CalendarDateTime Gregorian) (CalendarDateTime Gregorian -> String) String
  -> Pattern (Instant -> Instant) (Instant -> String) String
instantPattern = dimapP fromAdjustedInstant toUnadjustedInstant

-- | The ISO-8601 UTC instant pattern, @yyyy-MM-ddTHH:mm:ssZ@ (the trailing @Z@ marks it as UTC).
pInstant :: Pattern (Instant -> Instant) (Instant -> String) String
pInstant = instantPattern (ps <% char 'Z')

-- | Like 'pInstant' but carried down to the nanosecond, @yyyy-MM-ddTHH:mm:ss.fffffffffZ@.
pInstantNano :: Pattern (Instant -> Instant) (Instant -> String) String
pInstantNano = instantPattern (po <% char 'Z')
