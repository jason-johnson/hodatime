{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :  Data.HodaTime.Pattern.Locale
-- Copyright   :  (C) 2016 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  portable (Linux, macOS, Windows)
--
-- Compiles the @strftime@ layout strings captured in a 'Locale' by "Data.HodaTime.Locale" (the operating system's
-- @D_FMT@ \/ @T_FMT@ on POSIX, translated from the equivalent Windows /picture/ strings) into hodatime 'Pattern's, so a
-- date or time can be formatted and parsed using the machine's own conventions.
--
-- ==== __Using the machine's own layout__
--
-- 'localeDatePattern' turns a locale's @D_FMT@ into a pattern, so the /same/ date renders the way each culture writes
-- it — month-first in the US, day-first in Germany (here @march15@ is 15 March 2020):
--
-- > do us  <- localeByName "en_US.UTF-8"
-- >    de  <- localeByName "de_DE.UTF-8"
-- >    usP <- localeDatePattern us
-- >    deP <- localeDatePattern de
-- >    pure (format usP march15, format deP march15)       -- ("03/15/2020", "15.03.2020")
--
-- Use 'currentLocale' instead of 'localeByName' to follow the machine's own @LC_TIME@ setting, and 'parse' with the
-- same pattern to read that layout back:
--
-- > do loc <- currentLocale
-- >    p   <- localeDatePattern loc                        -- the current locale's short-date layout
-- >    pure (format p march15) >>= parse p                 -- round-trips in whatever order the locale uses
--
-- 'localeTimePattern' does the same for the time-of-day layout (@T_FMT@), and 'localeDateTimePattern' for the combined
-- date-and-time layout (@D_T_FMT@) as a 'CalendarDateTime'.
--
-- ==== __On time zones__
--
-- 'localeDateTimePattern' /deliberately ignores/ the time zone.  Most @D_T_FMT@ strings end with @%Z@ (a zone
-- abbreviation such as @CEST@) or @%z@ (a numeric offset); a 'CalendarDateTime' is /civil/ time with no zone attached,
-- so there is nothing to render there and nothing to interpret, and those specifiers are dropped from the compiled
-- pattern.  In particular a zone-less datetime is /not/ assumed to be UTC — treating civil time as UTC is exactly the
-- accidental coupling the library is built to avoid; turning a 'CalendarDateTime' into an absolute instant always
-- requires you to attach an offset or time zone /on purpose/.
--
-- When you /do/ want the zone, use 'parseZonedDateTime' (below): it parses the locale's zoned layout into a
-- 'Data.HodaTime.ZonedDateTime.ZonedDateTime', capturing the @%Z@ abbreviation and resolving it through a /provider/
-- you supply (abbreviations are ambiguous, so the caller owns that mapping).  It requires the layout to contain a zone,
-- throwing 'ZonelessLayoutException' otherwise — a layout with no zone is not a zoned value.
--
-- A layout that instead carries a /numeric/ offset (@%z@, e.g. @+0200@) is unambiguous, so
-- 'localeOffsetDateTimePattern' compiles it into an ordinary, pure, bidirectional
-- 'Data.HodaTime.OffsetDateTime.OffsetDateTime' pattern (used with 'parse' and 'format', no provider needed), throwing
-- 'OffsetlessLayoutException' if the layout has no @%z@.
module Data.HodaTime.Pattern.Locale
(
   StrftimeError(..)
  ,ZonelessLayoutException(..)
  ,OffsetlessLayoutException(..)
  ,localeDatePattern
  ,localeTimePattern
  ,localeDateTimePattern
  ,localeOffsetDateTimePattern
  ,parseZonedDateTime
)
where

import Data.HodaTime.Pattern.Internal (Pattern(..), (<%), string)
import Data.HodaTime.Pattern.CalendarDate (pyyyy, pyy, pMM, pdd, pdaySpace, pMMMM', pMMM', pdddd', pddd')
import Data.HodaTime.Pattern.OffsetDateTime (offsetDateTimePattern)
import Data.HodaTime.Pattern.Offset (pOffsetCompact)
import Data.HodaTime.Pattern.LocalTime (pHH, phh, phhSpace, pmm, pss, ppp')
import Data.HodaTime.Pattern.ZonedDateTime.Internal (parseZonedDateTimeWith)
import Data.HodaTime.Locale.Internal (Locale(..))
import Data.HodaTime.CalendarDateTime.Internal (HasDate, DoW, CalendarDateTime, IsCalendar)
import Data.HodaTime.LocalTime.Internal (HasLocalTime)
import Data.HodaTime.ZonedDateTime (ZonedDateTime)
import Data.HodaTime.OffsetDateTime (OffsetDateTime)
import Data.HodaTime.TimeZone (TimeZone)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TLB
import qualified Text.Parsec as P (string)
import Formatting (later)

-- | Raised when a layout string uses a @strftime@ conversion that the compiler does not implement.
data StrftimeError
  = UnsupportedSpecifier Char   -- ^ a conversion we do not support here (e.g. @%Z@, @%V@, or a width\/flag like @%-d@)
  | DanglingPercent             -- ^ the layout string ended with a bare @%@
  deriving (Eq, Show, Typeable)

instance Exception StrftimeError

-- | A literal chunk of the layout as a field pattern: it consumes\/emits the given text and leaves the value untouched.
--   Because it has the ordinary field-pattern shape it composes with '<>' like any other field, so literals anywhere in
--   the layout (including at the very start) need no special handling.
litField :: String -> Pattern (a -> a) (a -> String) String
litField s = Pattern (id <$ P.string s) (later (const (TLB.fromText (T.pack s))))

-- | A single token of a layout string, after composite specifiers have been expanded.
data Tok = Lit Char | Conv Char

-- | A run of literal characters or a single conversion specifier.
data Frag = LitRun String | ConvF Char

-- | Tokenise a layout string, expanding the composite specifiers (@%T@, @%R@, @%r@, @%F@, @%D@) into their parts.
tokenize :: String -> Either StrftimeError [Tok]
tokenize [] = Right []
tokenize ('%':c:rest) = case c of
  '%' -> (Lit '%' :)  <$> tokenize rest
  'n' -> (Lit '\n' :) <$> tokenize rest
  't' -> (Lit '\t' :) <$> tokenize rest
  'T' -> tokenize ("%H:%M:%S" ++ rest)
  'R' -> tokenize ("%H:%M" ++ rest)
  'r' -> tokenize ("%I:%M:%S %p" ++ rest)
  'F' -> tokenize ("%Y-%m-%d" ++ rest)
  'D' -> tokenize ("%m/%d/%y" ++ rest)
  _   -> (Conv c :) <$> tokenize rest
tokenize ['%'] = Left DanglingPercent
tokenize (c:rest) = (Lit c :) <$> tokenize rest

-- | Merge adjacent literal characters into runs so each becomes a single 'litField'.
toFrags :: [Tok] -> [Frag]
toFrags = foldr step []
  where
    step (Lit c)  (LitRun s : fs) = LitRun (c : s) : fs
    step (Lit c)  fs              = LitRun [c] : fs
    step (Conv c) fs              = ConvF c : fs

-- | Assemble a list of fragments into a pattern, given a mapping from conversion specifiers to field patterns.
assemble
  :: (Char -> Either StrftimeError (Pattern (a -> a) (a -> String) String))
  -> [Frag]
  -> Either StrftimeError (Pattern (a -> a) (a -> String) String)
assemble mapConv frags = do
  ps <- mapM toPat frags
  case ps of
    [] -> Right (litField "")
    _  -> Right (foldr1 (<>) ps)
  where
    toPat (LitRun s) = Right (litField s)
    toPat (ConvF c)  = mapConv c

-- | Compile a layout string into a pattern, given a specifier mapping.
compileWith
  :: (Char -> Either StrftimeError (Pattern (a -> a) (a -> String) String))
  -> String
  -> Either StrftimeError (Pattern (a -> a) (a -> String) String)
compileWith mapConv fmtStr = tokenize fmtStr >>= assemble mapConv . toFrags

-- | As 'compileWith' but first drops the zone specifiers (@%Z@\/@%z@) and a single preceding space; used for the
--   combined date-and-time layout, whose 'CalendarDateTime' target has no zone.
compileDroppingZones
  :: (Char -> Either StrftimeError (Pattern (a -> a) (a -> String) String))
  -> String
  -> Either StrftimeError (Pattern (a -> a) (a -> String) String)
compileDroppingZones mapConv fmtStr = tokenize fmtStr >>= assemble mapConv . stripZones . toFrags

-- | Drop the zone specifiers (@%Z@\/@%z@) and a single preceding space: a 'CalendarDateTime' has no zone to show.
stripZones :: [Frag] -> [Frag]
stripZones [] = []
stripZones (LitRun s : ConvF c : rest)
  | isZone c  = [LitRun s' | not (null s')] ++ stripZones rest
  where s' = reverse (dropWhile (== ' ') (reverse s))
stripZones (ConvF c : rest)
  | isZone c  = stripZones rest
stripZones (f : rest) = f : stripZones rest

isZone :: Char -> Bool
isZone c = c == 'Z' || c == 'z'

dateConv :: (HasDate d, Enum (DoW d)) => Locale -> Char -> Either StrftimeError (Pattern (d -> d) (d -> String) String)
dateConv loc c = case c of
  'Y' -> Right pyyyy
  'y' -> Right pyy
  'm' -> Right pMM
  'd' -> Right pdd
  'e' -> Right pdaySpace
  'B' -> Right (pMMMM' loc)
  'b' -> Right (pMMM' loc)
  'h' -> Right (pMMM' loc)
  'A' -> Right (pdddd' loc)
  'a' -> Right (pddd' loc)
  _   -> Left (UnsupportedSpecifier c)

timeConv :: HasLocalTime lt => Locale -> Char -> Either StrftimeError (Pattern (lt -> lt) (lt -> String) String)
timeConv loc c = case c of
  'H' -> Right pHH
  'I' -> Right phh
  'l' -> Right phhSpace
  'M' -> Right pmm
  'S' -> Right pss
  'p' -> Right (ppp' loc)
  _   -> Left (UnsupportedSpecifier c)

-- | Compile an explicit @strftime@ date layout against a 'Locale' (the locale supplies month\/weekday names for @%B@,
--   @%b@, @%A@, @%a@).  Throws a 'StrftimeError' on an unsupported specifier.
compileDatePattern :: (MonadThrow m, HasDate d, Enum (DoW d)) => Locale -> String -> m (Pattern (d -> d) (d -> String) String)
compileDatePattern loc = either throwM return . compileWith (dateConv loc)

-- | Compile an explicit @strftime@ time layout against a 'Locale' (the locale supplies the AM\/PM designators for
--   @%p@).  Throws a 'StrftimeError' on an unsupported specifier.
compileTimePattern :: (MonadThrow m, HasLocalTime lt) => Locale -> String -> m (Pattern (lt -> lt) (lt -> String) String)
compileTimePattern loc = either throwM return . compileWith (timeConv loc)

-- | The locale's short date pattern, compiled from its short-date layout (@rawDateFormat@; @D_FMT@ on POSIX).
localeDatePattern :: (MonadThrow m, HasDate d, Enum (DoW d)) => Locale -> m (Pattern (d -> d) (d -> String) String)
localeDatePattern loc = compileDatePattern loc (rawDateFormat loc)

-- | The locale's time pattern, compiled from its time layout (@rawTimeFormat@; @T_FMT@ on POSIX).
localeTimePattern :: (MonadThrow m, HasLocalTime lt) => Locale -> m (Pattern (lt -> lt) (lt -> String) String)
localeTimePattern loc = compileTimePattern loc (rawTimeFormat loc)

-- | Combined date-and-time mapping over 'CalendarDateTime': a date specifier resolves to its date field and a time
--   specifier to its time field (both are valid on a 'CalendarDateTime', which is 'HasDate' and 'HasLocalTime').
dateTimeConv :: (IsCalendar cal, Enum (DoW (CalendarDateTime cal))) => Locale -> Char -> Either StrftimeError (Pattern (CalendarDateTime cal -> CalendarDateTime cal) (CalendarDateTime cal -> String) String)
dateTimeConv loc c = case dateConv loc c of
  Right p -> Right p
  Left _  -> timeConv loc c

-- | The locale's combined date-and-time pattern, compiled from its combined layout (@rawDateTimeFormat@; @D_T_FMT@ on
--   POSIX) as a 'CalendarDateTime'.  The zone specifiers @%Z@\/@%z@ are dropped — see the note on time zones in the
--   module header.
localeDateTimePattern :: (MonadThrow m, IsCalendar cal, Enum (DoW (CalendarDateTime cal))) => Locale -> m (Pattern (CalendarDateTime cal -> CalendarDateTime cal) (CalendarDateTime cal -> String) String)
localeDateTimePattern loc = either throwM return (compileDroppingZones (dateTimeConv loc) (rawDateTimeFormat loc))

-- | Thrown by 'parseZonedDateTime' when the locale's @D_T_FMT@ has no zone (@%Z@): such a layout describes civil time,
--   not a zoned value, so use 'localeDateTimePattern' for it instead.
newtype ZonelessLayoutException = ZonelessLayout String   -- ^ carries the offending locale's id
  deriving (Eq, Show, Typeable)

instance Exception ZonelessLayoutException

-- | Parse a zoned date\/time written in the locale's @D_T_FMT@ into a 'ZonedDateTime'.  The local part is read from the
--   layout and the trailing @%Z@ token is captured and handed to the /provider/ (abbreviations such as @CEST@ are
--   ambiguous, so you supply the mapping to a real 'TimeZone'); the /resolver/ then decides skipped\/ambiguous local
--   times (e.g. @fromCalendarDateTimeStrictly@).  Throws 'ZonelessLayoutException' if the locale's layout has no @%Z@
--   (a layout with no zone is not a zoned value).
parseZonedDateTime
  :: (MonadThrow m, IsCalendar cal, Enum (DoW (CalendarDateTime cal)))
  => (String -> m TimeZone)
  -> (CalendarDateTime cal -> TimeZone -> m (ZonedDateTime cal))
  -> Locale
  -> String
  -> m (ZonedDateTime cal)
parseZonedDateTime provider resolve loc s
  | hasZoneSpecifier (rawDateTimeFormat loc) = localeDateTimePattern loc >>= \localPat -> parseZonedDateTimeWith localPat provider resolve s
  | otherwise                                = throwM (ZonelessLayout (localeId loc))

-- | Does a @strftime@ layout contain the zone specifier @%Z@ (respecting @%%@)?
hasZoneSpecifier :: String -> Bool
hasZoneSpecifier ('%':'%':rest) = hasZoneSpecifier rest
hasZoneSpecifier ('%':'Z':_)    = True
hasZoneSpecifier ('%':_:rest)   = hasZoneSpecifier rest
hasZoneSpecifier (_:rest)       = hasZoneSpecifier rest
hasZoneSpecifier []             = False

-- | Thrown by 'localeOffsetDateTimePattern' when the locale's @D_T_FMT@ has no numeric offset (@%z@).
newtype OffsetlessLayoutException = OffsetlessLayout String   -- ^ carries the offending locale's id
  deriving (Eq, Show, Typeable)

instance Exception OffsetlessLayoutException

-- | Compile the locale's @D_T_FMT@ into an 'OffsetDateTime' pattern, using its numeric offset (@%z@, e.g. @+0200@).
--   Unlike 'parseZonedDateTime' this is a plain, /pure/, bidirectional pattern (drive it with 'Data.HodaTime.Pattern.parse'
--   and 'Data.HodaTime.Pattern.format') because a numeric offset is unambiguous — no zone provider or resolver is
--   needed.  Throws 'OffsetlessLayoutException' if the layout has no @%z@ (the abbreviation form @%Z@ is not an offset;
--   use 'parseZonedDateTime' for that).
localeOffsetDateTimePattern
  :: (MonadThrow m, IsCalendar cal, Enum (DoW (CalendarDateTime cal)))
  => Locale
  -> m (Pattern (OffsetDateTime cal -> OffsetDateTime cal) (OffsetDateTime cal -> String) String)
localeOffsetDateTimePattern loc
  | hasOffsetSpecifier fmt = either throwM return (compileOffsetDateTime (dateTimeConv loc) fmt)
  | otherwise              = throwM (OffsetlessLayout (localeId loc))
  where fmt = rawDateTimeFormat loc

-- | Split a @%z@-bearing layout into its local part and the offset, compile the local part, and pair it with the
--   compact-offset field.
compileOffsetDateTime
  :: IsCalendar cal
  => (Char -> Either StrftimeError (Pattern (CalendarDateTime cal -> CalendarDateTime cal) (CalendarDateTime cal -> String) String))
  -> String
  -> Either StrftimeError (Pattern (OffsetDateTime cal -> OffsetDateTime cal) (OffsetDateTime cal -> String) String)
compileOffsetDateTime dtConv fmtStr = do
  toks <- tokenize fmtStr
  let (before, rest) = break isOffsetFrag (toFrags toks)
  localPat <- assemble dtConv before
  trailing <- trailingLits (drop 1 rest)
  return (offsetDateTimePattern localPat (pOffsetCompact <% string trailing))

isOffsetFrag :: Frag -> Bool
isOffsetFrag (ConvF 'z') = True
isOffsetFrag _           = False

-- | The literal text following @%z@ (normally none); a further field there is unsupported.
trailingLits :: [Frag] -> Either StrftimeError String
trailingLits []                = Right ""
trailingLits (LitRun s : rest) = (s ++) <$> trailingLits rest
trailingLits (ConvF c : _)     = Left (UnsupportedSpecifier c)

-- | Does a @strftime@ layout contain the numeric-offset specifier @%z@ (respecting @%%@)?
hasOffsetSpecifier :: String -> Bool
hasOffsetSpecifier ('%':'%':rest) = hasOffsetSpecifier rest
hasOffsetSpecifier ('%':'z':_)    = True
hasOffsetSpecifier ('%':_:rest)   = hasOffsetSpecifier rest
hasOffsetSpecifier (_:rest)       = hasOffsetSpecifier rest
hasOffsetSpecifier []             = False
