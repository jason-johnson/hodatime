-- |
-- Module      :  Data.HodaTime.Locale
-- Copyright   :  (C) 2016 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- Provides culture-specific names (month and weekday names, AM\/PM designators) and layout strings for dates and times.
-- A 'Locale' can be read from the operating system's locale database — in the same spirit as the time-zone support,
-- the data lives on the machine and is read on demand rather than being bundled — or you can use one of the built-in
-- locales ('enUS', 'deDE', 'jaJP') when you want a fixed, pure one with no @IO@.
--
-- 'Locale' is /abstract/: you never construct one yourself, you obtain it from 'currentLocale', 'localeByName' or a
-- built-in, and then hand it to the culture-aware patterns.  Those live in "Data.HodaTime.Pattern.Locale" (whole-layout
-- patterns — @localeDatePattern@ \/ @localeTimePattern@) and in "Data.HodaTime.Pattern.CalendarDate" \/
-- "Data.HodaTime.Pattern.LocalTime" (individual name fields — @pMMMM'@, @pdddd'@, @ppp'@).
--
-- ==== __Getting a locale__
--
-- Three ways to obtain one — a built-in (pure, no @IO@), a specific installed locale by name, or the machine's own
-- current locale — each then handed to a culture-aware pattern:
--
-- > import Data.HodaTime.Locale (deDE, localeByName, currentLocale)
-- > import Data.HodaTime.Pattern (format)
-- > import Data.HodaTime.Pattern.Locale (localeDatePattern)
-- >
-- > -- a built-in: pure, formats the German way (15.03.2020)
-- > fromBuiltin = do
-- >   p <- localeDatePattern deDE
-- >   pure (format p someDate)
-- >
-- > -- a named locale installed on the machine
-- > fromName = do
-- >   loc <- localeByName "de_DE.UTF-8"
-- >   p   <- localeDatePattern loc
-- >   pure (format p someDate)
-- >
-- > -- whatever the machine's LC_TIME is set to
-- > fromCurrent = do
-- >   loc <- currentLocale
-- >   p   <- localeDatePattern loc
-- >   pure (format p someDate)
module Data.HodaTime.Locale
(
  -- * Locale
   Locale
  -- * Built-in locales
  ,enUS
  ,deDE
  ,jaJP
  -- * Reading the machine's locale
  ,currentLocale
  ,localeByName
  ,LocaleException(..)
  -- * Inspecting a locale
  ,localeId
  ,monthNames
  ,monthNamesShort
  ,dayNames
  ,dayNamesShort
  ,amName
  ,pmName
)
where

import Data.HodaTime.Locale.Internal (Locale, enUS, deDE, jaJP)
import qualified Data.HodaTime.Locale.Internal as I
import qualified Data.HodaTime.Locale.Platform as Platform
import Control.Exception (Exception, throwIO)

-- | Thrown by 'localeByName' when the requested locale is not installed on the machine.
newtype LocaleException = LocaleNotFound String
  deriving (Show)

instance Exception LocaleException

-- | Read the process's current locale, as selected by the environment (@LC_ALL@ \/ @LC_TIME@ \/ @LANG@), falling back
--   to the POSIX @C@ locale.
currentLocale :: IO Locale
currentLocale = Platform.loadCurrentLocale

-- | Read a specific locale by name, e.g. @\"de_DE.UTF-8\"@.  Throws 'LocaleNotFound' if it is not installed.
localeByName :: String -> IO Locale
localeByName name = Platform.loadLocaleByName name >>= maybe (throwIO (LocaleNotFound name)) return

-- The accessors below are read-only wrappers over the (hidden) representation, so a 'Locale' can be inspected but never
-- constructed or modified from outside the library.

-- | The identifier this locale was loaded from (e.g. @\"de_DE.UTF-8\"@), or the short code of a built-in.
localeId :: Locale -> String
localeId = I.localeId

-- | Full month names, January-first (12 entries for the Gregorian calendar).
monthNames :: Locale -> [String]
monthNames = I.monthNames

-- | Abbreviated month names, January-first.
monthNamesShort :: Locale -> [String]
monthNamesShort = I.monthNamesShort

-- | Full weekday names, Sunday-first (7 entries).
dayNames :: Locale -> [String]
dayNames = I.dayNames

-- | Abbreviated weekday names, Sunday-first.
dayNamesShort :: Locale -> [String]
dayNamesShort = I.dayNamesShort

-- | The AM designator (may be empty in 24-hour cultures such as German).
amName :: Locale -> String
amName = I.amName

-- | The PM designator (may be empty in 24-hour cultures such as German).
pmName :: Locale -> String
pmName = I.pmName
