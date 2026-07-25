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
-- === Formatting, and the two-step parse
--
-- Formatting is direct: 'pZonedDateTime' renders a 'ZonedDateTime' as its local date\/time plus the zone id.
--
-- Parsing is split in two, because building a 'ZonedDateTime' is effectful — it has to load the time-zone rules for
-- the parsed zone id and then /resolve/ the local time (which may be skipped or ambiguous), neither of which fits the
-- pure 'Data.HodaTime.Pattern.parse'.  So 'pZonedDateTimeInfo' parses (purely) into a 'ZonedDateTimeInfo' — the local
-- 'CalendarDateTime' plus the zone id — and 'resolveZonedDateTime' takes that structure together with a zone
-- /provider/ and a /resolver/ and produces the 'ZonedDateTime'.  'parseZonedDateTime' bundles the two.  Note that
-- 'pZonedDateTime' itself (which formats a fully-built 'ZonedDateTime') cannot be parsed directly — use
-- 'pZonedDateTimeInfo'.
----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.HodaTime.Pattern.ZonedDateTime
(
  -- * Types
   ZonedDateTimeInfo(..)
  -- * Standard Patterns
  ,pZonedDateTime
  ,pZonedDateTimeInfo
  -- * Parsing (effectful resolution)
  ,resolveZonedDateTime
  ,parseZonedDateTime
  -- * Custom Patterns
  --
  -- | Build a (format-only) 'ZonedDateTime' pattern from a 'CalendarDateTime' pattern and a zone-rendering function.
  ,zonedDateTimePattern
)
where

import Data.HodaTime.Pattern.Internal (Pattern(..), DefaultForParse(..), parse, format, char, (<%))
import Data.HodaTime.Pattern.CalendarDateTime (ps)
import Data.HodaTime.ZonedDateTime (ZonedDateTime, toCalendarDateTime, zoneId)
import Data.HodaTime.CalendarDateTime (CalendarDateTime)
import Data.HodaTime.CalendarDateTime.Internal (IsCalendar, Date)
import Data.HodaTime.TimeZone (TimeZone)
import Control.Monad.Catch (MonadThrow)
import Formatting (later)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TLB
import Data.Char (isSpace)
import Text.Parsec (parserFail, many1, satisfy, (<?>))

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
    par = parserFail "ZonedDateTime cannot be parsed directly; parse into a ZonedDateTimeInfo with pZonedDateTimeInfo, then use resolveZonedDateTime / parseZonedDateTime"
    fmt = later (\zdt -> TLB.fromText . T.pack $ format cdtPat (toCalendarDateTime zdt) ++ renderZone zdt)

-- | The ISO-8601 local date\/time followed by a space and the (unambiguous) IANA zone id, e.g.
--   @2024-04-23T09:00:00 Europe\/Zurich@.
pZonedDateTime :: IsCalendar cal => Pattern (ZonedDateTime cal -> ZonedDateTime cal) (ZonedDateTime cal -> String) String
pZonedDateTime = zonedDateTimePattern ps (\zdt -> " " ++ zoneId zdt)

-- | The pure result of parsing a zoned date\/time: the local (wall-clock) 'CalendarDateTime' together with the zone
--   identifier text.  Turning it into a 'ZonedDateTime' is effectful (loading the zone, resolving skipped\/ambiguous
--   local times), so that step is 'resolveZonedDateTime'.
data ZonedDateTimeInfo cal = ZonedDateTimeInfo
  { zdtLocal  :: CalendarDateTime cal   -- ^ the parsed local (wall-clock) date and time
  , zdtZoneId :: String                 -- ^ the parsed zone identifier, e.g. @Europe\/Zurich@
  }

deriving instance Eq (Date cal) => Eq (ZonedDateTimeInfo cal)
deriving instance Show (Date cal) => Show (ZonedDateTimeInfo cal)

instance IsCalendar cal => DefaultForParse (ZonedDateTimeInfo cal) where
  getDefault = ZonedDateTimeInfo getDefault ""

-- | Parse a zoned date\/time /purely/ into a 'ZonedDateTimeInfo' (local date\/time + zone id).  Use
--   'resolveZonedDateTime' (or 'parseZonedDateTime') to turn it into a 'ZonedDateTime'.  Its formatting matches
--   'pZonedDateTime'.
pZonedDateTimeInfo :: IsCalendar cal => Pattern (ZonedDateTimeInfo cal -> ZonedDateTimeInfo cal) (ZonedDateTimeInfo cal -> String) String
pZonedDateTimeInfo = Pattern par fmt
  where
    localPat = ps <% char ' '
    zoneToken = many1 (satisfy (not . isSpace)) <?> "zone id"
    build setCdt z = const (ZonedDateTimeInfo (setCdt getDefault) z)
    par = build <$> _patParse localPat <*> zoneToken
    fmt = later (\info -> TLB.fromText . T.pack $ format localPat (zdtLocal info) ++ zdtZoneId info)

-- | Turn a parsed 'ZonedDateTimeInfo' into a 'ZonedDateTime'.  The /provider/ loads the time zone for the parsed id
--   (e.g. 'Data.HodaTime.TimeZone.timeZone' in 'IO', or a pure lookup in a preloaded map), and the /resolver/ decides
--   what to do with that local time in that zone (e.g. 'Data.HodaTime.ZonedDateTime.fromCalendarDateTimeStrictly' or
--   @...Leniently@, or a custom policy) — including the skipped\/ambiguous cases.
resolveZonedDateTime
  :: Monad m
  => (String -> m TimeZone)                                        -- ^ zone provider
  -> (CalendarDateTime cal -> TimeZone -> m (ZonedDateTime cal))   -- ^ resolver policy
  -> ZonedDateTimeInfo cal
  -> m (ZonedDateTime cal)
resolveZonedDateTime provider resolve info = do
  tz <- provider (zdtZoneId info)
  resolve (zdtLocal info) tz

-- | Parse and resolve in one step: @'parse' 'pZonedDateTimeInfo'@ followed by 'resolveZonedDateTime'.
parseZonedDateTime
  :: (MonadThrow m, IsCalendar cal)
  => (String -> m TimeZone)
  -> (CalendarDateTime cal -> TimeZone -> m (ZonedDateTime cal))
  -> String
  -> m (ZonedDateTime cal)
parseZonedDateTime provider resolve s = parse pZonedDateTimeInfo s >>= resolveZonedDateTime provider resolve