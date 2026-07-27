{-# LANGUAGE ForeignFunctionInterface #-}

-- | Windows implementation of the locale reader, via @GetLocaleInfoEx@.
--
--   The name fields (month\/weekday names and AM\/PM designators, which power @pMMMM'@, @pddd'@, @ppp'@ and the reader)
--   are fully populated.  The @raw*Format@ fields, however, hold Windows /picture/ strings (e.g. @dd.MM.yyyy@), which
--   are __not__ POSIX @strftime@ — so the layout-compiling patterns in "Data.HodaTime.Pattern.Locale"
--   (@localeDatePattern@ and friends) do not yet work on a locale /read on Windows/.  The built-in locales
--   (@enUS@\/@deDE@\/@jaJP@) carry @strftime@ and work everywhere.  Translating the picture strings to @strftime@ is a
--   follow-up.
module Data.HodaTime.Locale.Platform
(
  loadCurrentLocale
 ,loadLocaleByName
)
where

import Data.HodaTime.Locale.Internal (Locale(..))
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Types (CInt(..), CWchar)
import Foreign.Marshal.Array (allocaArray, peekArray, withArray0)
import Data.Bits ((.&.))
import Data.Char (chr, ord)

#include <windows.h>

-- @int GetLocaleInfoEx(LPCWSTR lpLocaleName, LCTYPE LCType, LPWSTR lpLCData, int cchData)@.  (Windows CI is x86-64,
-- where @ccall@ is the sole calling convention; a 32-bit build would need @stdcall@.)
foreign import ccall unsafe "GetLocaleInfoEx"
  c_GetLocaleInfoEx :: Ptr CWchar -> CInt -> Ptr CWchar -> CInt -> IO CInt

-- LCTYPE bases (the numbered families are consecutive in @winnls.h@, so 'enumFrom' walks them).
lOCALE_SMONTHNAME1, lOCALE_SABBREVMONTHNAME1, lOCALE_SDAYNAME1, lOCALE_SABBREVDAYNAME1 :: CInt
lOCALE_SMONTHNAME1       = #{const LOCALE_SMONTHNAME1}
lOCALE_SABBREVMONTHNAME1 = #{const LOCALE_SABBREVMONTHNAME1}
lOCALE_SDAYNAME1         = #{const LOCALE_SDAYNAME1}
lOCALE_SABBREVDAYNAME1   = #{const LOCALE_SABBREVDAYNAME1}

lOCALE_S1159, lOCALE_S2359, lOCALE_SSHORTDATE, lOCALE_STIMEFORMAT :: CInt
lOCALE_S1159       = #{const LOCALE_S1159}
lOCALE_S2359       = #{const LOCALE_S2359}
lOCALE_SSHORTDATE  = #{const LOCALE_SSHORTDATE}
lOCALE_STIMEFORMAT = #{const LOCALE_STIMEFORMAT}
#if defined(LOCALE_SNAME)
lOCALE_SNAME :: CInt
lOCALE_SNAME       = #{const LOCALE_SNAME}
#endif

-- | Query one @LCTYPE@ from the given locale (a pointer to a wide name, or 'nullPtr' for the current locale) and decode
--   it as a 'String' (UTF-16, BMP — locale strings never use surrogate pairs).
getInfo :: Ptr CWchar -> CInt -> IO String
getInfo loc lctype = do
  n <- c_GetLocaleInfoEx loc lctype nullPtr 0
  if n <= 0
    then return ""
    else allocaArray (fromIntegral n) $ \buf -> do
      _  <- c_GetLocaleInfoEx loc lctype buf n
      ws <- peekArray (fromIntegral n) buf
      return . map (chr . (0xFFFF .&.) . fromIntegral) . takeWhile (/= 0) $ ws

-- | Run an action with a wide, null-terminated locale name, or with 'nullPtr' for the current locale.
withLocaleName :: Maybe String -> (Ptr CWchar -> IO a) -> IO a
withLocaleName Nothing  act = act nullPtr
withLocaleName (Just s) act = withArray0 0 (map (fromIntegral . ord) s) act

-- | POSIX @\"C\"@\/@\"POSIX\"@ (and the empty name) map to the Windows /invariant/ locale (@L\"\"@).
normalizeName :: String -> String
normalizeName n
  | n `elem` ["C", "POSIX", ""] = ""
  | otherwise                   = n

buildLocale :: String -> Ptr CWchar -> IO Locale
buildLocale lid loc = do
  monsFull    <- mapM (getInfo loc) (take 12 [lOCALE_SMONTHNAME1 ..])
  monsAbbr    <- mapM (getInfo loc) (take 12 [lOCALE_SABBREVMONTHNAME1 ..])
  daysMon     <- mapM (getInfo loc) (take 7  [lOCALE_SDAYNAME1 ..])        -- Windows is Monday-first
  daysAbbrMon <- mapM (getInfo loc) (take 7  [lOCALE_SABBREVDAYNAME1 ..])
  am    <- getInfo loc lOCALE_S1159
  pm    <- getInfo loc lOCALE_S2359
  sdate <- getInfo loc lOCALE_SSHORTDATE
  stime <- getInfo loc lOCALE_STIMEFORMAT
  return Locale
    { localeId          = lid
    , monthNames        = monsFull
    , monthNamesShort   = monsAbbr
    , dayNames          = sundayFirst daysMon
    , dayNamesShort     = sundayFirst daysAbbrMon
    , amName            = am
    , pmName            = pm
    , rawDateFormat     = sdate
    , rawTimeFormat     = stime
    , rawDateTimeFormat = sdate ++ " " ++ stime
    }
  where
    sundayFirst xs = last xs : init xs   -- [Mon .. Sun] -> [Sun, Mon .. Sat]

-- | Read a specific locale by (Windows or normalized POSIX) name; 'Nothing' if it is not a valid locale.
loadLocaleByName :: String -> IO (Maybe Locale)
loadLocaleByName name = withLocaleName (Just (normalizeName name)) $ \loc -> do
  n <- c_GetLocaleInfoEx loc lOCALE_SMONTHNAME1 nullPtr 0   -- validity probe
  if n <= 0 then return Nothing else Just <$> buildLocale name loc

-- | Read the current (user default) locale.
loadCurrentLocale :: IO Locale
loadCurrentLocale = do
#if defined(LOCALE_SNAME)
  lid <- withLocaleName Nothing (`getInfo` lOCALE_SNAME)
  withLocaleName Nothing (buildLocale (if null lid then "C" else lid))
#else
  withLocaleName Nothing (buildLocale "C")
#endif
