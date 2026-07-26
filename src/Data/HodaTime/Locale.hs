-- |
-- Module      :  Data.HodaTime.Locale
-- Copyright   :  (C) 2016 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX (Linux, macOS); not yet implemented on Windows
--
-- Reads culture-specific names (month and weekday names, AM\/PM designators) from the operating system's locale
-- database, in the same spirit as the time-zone support: the data lives on the machine and is read on demand rather
-- than being bundled into the library.
--
-- The values feed the /culture-aware/ pattern variants in "Data.HodaTime.Pattern.CalendarDate" and
-- "Data.HodaTime.Pattern.LocalTime" (e.g. @pMMMM'@, @pdddd'@, @ppp'@), which take a 'Locale' instead of using the
-- calendar's built-in (English) constructor names.
--
-- ==== __Example__
--
-- > import Data.HodaTime.Locale
-- > import Data.HodaTime.Pattern
-- > import Data.HodaTime.Pattern.CalendarDate (pdd, pMMMM', pyyyy)
-- >
-- > -- read the machine's current locale and format a date's month name in it
-- > example = do
-- >   loc <- currentLocale
-- >   let p = pdd <% char ' ' <> pMMMM' loc <% char ' ' <> pyyyy
-- >   pure (format p someDate)
module Data.HodaTime.Locale
(
   Locale(..)
  ,currentLocale
  ,localeByName
  ,LocaleException(..)
)
where

import Data.HodaTime.Locale.Internal (Locale(..))
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
