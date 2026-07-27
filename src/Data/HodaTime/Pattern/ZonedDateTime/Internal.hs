{-# LANGUAGE FlexibleContexts #-}

-- | Shared, non-user-facing machinery for parsing 'Data.HodaTime.ZonedDateTime.ZonedDateTime' values.  It lives in its
--   own module so that both "Data.HodaTime.Pattern.ZonedDateTime" (the ISO parser) and "Data.HodaTime.Pattern.Locale"
--   (the locale-driven parser) can build on it without exposing the intermediate 'ZonedDateTimeInfo' to users.
module Data.HodaTime.Pattern.ZonedDateTime.Internal
(
   ZonedDateTimeInfo(..)
  ,resolveZonedDateTime
  ,parseZonedDateTimeWith
)
where

import Data.HodaTime.Pattern.Internal (Pattern(..), DefaultForParse(..), parse)
import Data.HodaTime.ZonedDateTime (ZonedDateTime)
import Data.HodaTime.CalendarDateTime (CalendarDateTime)
import Data.HodaTime.CalendarDateTime.Internal (IsCalendar)
import Data.HodaTime.TimeZone (TimeZone)
import Control.Monad.Catch (MonadThrow)
import Formatting (later)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TLB
import Data.Char (isSpace)
import Text.Parsec (many1, satisfy, skipMany, (<?>))

-- | The pure result of parsing a zoned date\/time: the local (wall-clock) 'CalendarDateTime' together with the zone
--   token (an IANA id for the ISO parser, or a @%Z@ abbreviation for the locale parser).  Users never see this; the
--   parse functions turn text straight into a 'ZonedDateTime'.
data ZonedDateTimeInfo cal = ZonedDateTimeInfo
  { zdtLocal  :: CalendarDateTime cal
  , zdtZoneId :: String
  }

instance IsCalendar cal => DefaultForParse (ZonedDateTimeInfo cal) where
  getDefault = ZonedDateTimeInfo getDefault ""

-- | Turn a parsed 'ZonedDateTimeInfo' into a 'ZonedDateTime': the /provider/ loads the 'TimeZone' for the parsed token
--   and the /resolver/ maps the local time into the zone (deciding skipped\/ambiguous cases).
resolveZonedDateTime
  :: Monad m
  => (String -> m TimeZone)
  -> (CalendarDateTime cal -> TimeZone -> m (ZonedDateTime cal))
  -> ZonedDateTimeInfo cal
  -> m (ZonedDateTime cal)
resolveZonedDateTime provider resolve info = do
  tz <- provider (zdtZoneId info)
  resolve (zdtLocal info) tz

-- | Parse a zoned date\/time using @localPat@ for the local part, then a trailing zone token (any run of non-space
--   characters, after optional whitespace), and resolve it with the given provider and resolver.  The local pattern's
--   own format side is irrelevant here — only its parser is used.
parseZonedDateTimeWith
  :: (MonadThrow m, IsCalendar cal)
  => Pattern (CalendarDateTime cal -> CalendarDateTime cal) b String
  -> (String -> m TimeZone)
  -> (CalendarDateTime cal -> TimeZone -> m (ZonedDateTime cal))
  -> String
  -> m (ZonedDateTime cal)
parseZonedDateTimeWith localPat provider resolve s = parse infoPat s >>= resolveZonedDateTime provider resolve
  where
    infoPat = Pattern par fmt
    par = build <$> _patParse localPat <*> zoneToken
    zoneToken = skipMany (satisfy isSpace) *> (many1 (satisfy (not . isSpace)) <?> "zone")
    build setCdt z = const (ZonedDateTimeInfo (setCdt getDefault) z)
    fmt = later (\info -> TLB.fromText . T.pack $ zdtZoneId info)
