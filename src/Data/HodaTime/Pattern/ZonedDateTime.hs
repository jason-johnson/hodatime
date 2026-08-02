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
-- === Formatting, and effectful parsing
--
-- Formatting is direct: @pZonedDateTime@ renders a 'ZonedDateTime' as its local date\/time plus the zone id.
--
-- Parsing cannot use the pure @parse@, because building a 'ZonedDateTime' is effectful: it has to load the time-zone
-- rules for the parsed zone id and then /resolve/ the local time (which may be skipped or ambiguous).  Use
-- @parseZonedDateTime@ instead — you supply a zone /provider/ (e.g. @timeZone@ in 'IO', or a pure lookup) and a
-- /resolver/ (e.g. @fromCalendarDateTimeStrictly@).  Calling @parse@ on @pZonedDateTime@ is a type error by design;
-- @parseZonedDateTime@ is the only way to parse one.
----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
module Data.HodaTime.Pattern.ZonedDateTime
(
  -- * Standard Patterns
   pZonedDateTime
  -- * Parsing
  ,parseZonedDateTime
  -- * Custom Patterns
  --
  -- | Build a (format-only) 'ZonedDateTime' pattern from a 'CalendarDateTime' pattern and a zone-rendering function.
  ,zonedDateTimePattern
)
where

import Data.HodaTime.Pattern.Internal (Pattern(..), format)
import Data.HodaTime.Pattern.ZonedDateTime.Internal (parseZonedDateTimeWith)
import Data.HodaTime.Pattern.CalendarDateTime (ps)
import Data.HodaTime.ZonedDateTime (ZonedDateTime, toCalendarDateTime, zoneId)
import Data.HodaTime.CalendarDateTime (CalendarDateTime)
import Data.HodaTime.CalendarDateTime.Internal (IsCalendar, Month)
import Data.HodaTime.TimeZone (TimeZone)
import Control.Monad.Catch (MonadThrow)
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
    par = parserFail "ZonedDateTime cannot be parsed with parse; use parseZonedDateTime (it is effectful -- it loads the zone and resolves the local time)"
    fmt = later (\zdt -> TLB.fromText . T.pack $ format cdtPat (toCalendarDateTime zdt) ++ renderZone zdt)

-- | The ISO-8601 local date\/time followed by a space and the (unambiguous) IANA zone id, e.g.
--   @2024-04-23T09:00:00 Europe\/Zurich@.
pZonedDateTime :: (IsCalendar cal, Enum (Month cal)) => Pattern (ZonedDateTime cal -> ZonedDateTime cal) (ZonedDateTime cal -> String) String
pZonedDateTime = zonedDateTimePattern ps (\zdt -> " " ++ zoneId zdt)

-- | Parse a zoned date\/time and resolve it to a 'ZonedDateTime'.  You supply a zone /provider/ (which loads the
--   'TimeZone' for the parsed id — @timeZone@ in 'IO', or a pure lookup) and a /resolver/ (which turns the local time
--   into a 'ZonedDateTime', deciding the skipped\/ambiguous cases — e.g. @fromCalendarDateTimeStrictly@).  This is the
--   only way to parse a 'ZonedDateTime'; the pure @parse@ cannot (it is a type error on 'pZonedDateTime').
parseZonedDateTime
  :: (MonadThrow m, IsCalendar cal, Enum (Month cal))
  => (String -> m TimeZone)
  -> (CalendarDateTime cal -> TimeZone -> m (ZonedDateTime cal))
  -> String
  -> m (ZonedDateTime cal)
parseZonedDateTime = parseZonedDateTimeWith ps