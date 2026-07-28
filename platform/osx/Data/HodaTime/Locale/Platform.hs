-- | macOS implementation of the locale reader.  macOS exposes the POSIX per-locale query API, so this simply delegates
--   to the shared "Data.HodaTime.Locale.Posix" shim.
module Data.HodaTime.Locale.Platform
(
  loadCurrentLocale
 ,loadLocaleByName
)
where

import Data.HodaTime.Locale.Internal (Locale)
import qualified Data.HodaTime.Locale.Posix as P

loadCurrentLocale :: IO Locale
loadCurrentLocale = P.loadCurrentLocale

loadLocaleByName :: String -> IO (Maybe Locale)
loadLocaleByName = P.loadLocaleByName
