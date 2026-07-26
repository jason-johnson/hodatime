-- | Windows implementation of the locale reader.
--
--   NOTE: this is a placeholder for the Phase 1 (POSIX-only) locale work.  A real implementation should read the locale
--   database via @GetLocaleInfoEx@ (@LOCALE_SMONTHNAME1@, @LOCALE_SDAYNAME1@, @LOCALE_S1159@ \/ @LOCALE_S2359@, ...).
--   Until then, locale reading is not available on Windows.
module Data.HodaTime.Locale.Platform
(
  loadCurrentLocale
 ,loadLocaleByName
)
where

import Data.HodaTime.Locale.Internal (Locale)

loadCurrentLocale :: IO Locale
loadCurrentLocale = ioError (userError "Data.HodaTime.Locale: reading the machine locale is not yet implemented on Windows")

loadLocaleByName :: String -> IO (Maybe Locale)
loadLocaleByName _ = return Nothing
