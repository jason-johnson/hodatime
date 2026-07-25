-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Pattern.ZonedDateTime
-- Copyright   :  (C) 2017 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- Patterns for 'ZonedDateTime'.
--
-- === NOTE: formatting only (for now)
--
-- These patterns currently support /formatting/ only.  Parsing a 'ZonedDateTime' is fundamentally effectful: it has
-- to load the time-zone rules for the parsed zone id (an @IO@ action) and then /resolve/ the local time within that
-- zone, which may be skipped or ambiguous.  That does not fit the pure 'Data.HodaTime.Pattern.parse' (whose value
-- construction is pure), so a dedicated effectful @parseZonedDateTime@ — taking a zone provider and a resolver — is
-- planned.  Attempting to parse with these patterns fails with a message pointing here.
----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
module Data.HodaTime.Pattern.ZonedDateTime
(
  -- * Standard Patterns
   pZonedDateTime
  -- * Custom Patterns
  --
  -- | Build a (format-only) 'ZonedDateTime' pattern from a 'CalendarDateTime' pattern and a zone-rendering function.
  ,zonedDateTimePattern
)
where

import Data.HodaTime.Pattern.Internal (Pattern(..), format)
import Data.HodaTime.Pattern.CalendarDateTime (ps)
import Data.HodaTime.ZonedDateTime (ZonedDateTime, toCalendarDateTime, zoneId)
import Data.HodaTime.CalendarDateTime (CalendarDateTime)
import Data.HodaTime.CalendarDateTime.Internal (IsCalendar)
import Formatting (later)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TLB
import Text.Parsec (parserFail)

-- | Format a 'ZonedDateTime' using the given 'CalendarDateTime' pattern for the local date\/time, followed by the
--   result of the zone-rendering function (e.g. a leading space and the zone id).
--
--   Formatting only: the parse side deliberately fails (see the module note), so the pure
--   'Data.HodaTime.Pattern.parse' cannot silently produce a wrong 'ZonedDateTime'.
zonedDateTimePattern
  :: Pattern (CalendarDateTime cal -> CalendarDateTime cal) (CalendarDateTime cal -> String) String
  -> (ZonedDateTime cal -> String)
  -> Pattern (ZonedDateTime cal -> ZonedDateTime cal) (ZonedDateTime cal -> String) String
zonedDateTimePattern cdtPat renderZone = Pattern par fmt
  where
    par = parserFail "ZonedDateTime cannot be parsed with 'parse'; an effectful parseZonedDateTime is planned (see Data.HodaTime.Pattern.ZonedDateTime)"
    fmt = later (\zdt -> TLB.fromText . T.pack $ format cdtPat (toCalendarDateTime zdt) ++ renderZone zdt)

-- | The ISO-8601 local date\/time followed by a space and the (unambiguous) IANA zone id, e.g.
--   @2024-04-23T09:00:00 Europe\/Zurich@.
pZonedDateTime :: IsCalendar cal => Pattern (ZonedDateTime cal -> ZonedDateTime cal) (ZonedDateTime cal -> String) String
pZonedDateTime = zonedDateTimePattern ps (\zdt -> " " ++ zoneId zdt)