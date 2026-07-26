{-# LANGUAGE ForeignFunctionInterface #-}

-- | POSIX implementation of the locale reader, shared by the Linux and macOS platform shims.  It reads the locale
--   database through the C library's per-locale query functions (@newlocale@ \/ @nl_langinfo_l@ \/ @freelocale@),
--   which are thread-safe and never mutate the global @setlocale@ state.
module Data.HodaTime.Locale.Posix
(
  loadCurrentLocale
 ,loadLocaleByName
)
where

import Data.HodaTime.Locale.Internal (Locale(..))
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Types (CInt(..))
import Foreign.C.String (CString, withCString)
import Control.Exception (bracket)
import Control.Monad (when)
import System.Environment (lookupEnv)
import Data.Maybe (catMaybes)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <locale.h>
#include <langinfo.h>

-- | An opaque POSIX @locale_t@ handle.
type CLocale = Ptr ()

foreign import ccall unsafe "newlocale"
  c_newlocale :: CInt -> CString -> CLocale -> IO CLocale

foreign import ccall unsafe "freelocale"
  c_freelocale :: CLocale -> IO ()

foreign import ccall unsafe "nl_langinfo_l"
  c_nl_langinfo_l :: CInt -> CLocale -> IO CString

lcAllMask :: CInt
lcAllMask = #{const LC_ALL_MASK}

monthItems :: [CInt]
monthItems =
  [ #{const MON_1},  #{const MON_2},  #{const MON_3},  #{const MON_4}
  , #{const MON_5},  #{const MON_6},  #{const MON_7},  #{const MON_8}
  , #{const MON_9},  #{const MON_10}, #{const MON_11}, #{const MON_12} ]

abMonthItems :: [CInt]
abMonthItems =
  [ #{const ABMON_1},  #{const ABMON_2},  #{const ABMON_3},  #{const ABMON_4}
  , #{const ABMON_5},  #{const ABMON_6},  #{const ABMON_7},  #{const ABMON_8}
  , #{const ABMON_9},  #{const ABMON_10}, #{const ABMON_11}, #{const ABMON_12} ]

dayItems :: [CInt]
dayItems =
  [ #{const DAY_1}, #{const DAY_2}, #{const DAY_3}, #{const DAY_4}
  , #{const DAY_5}, #{const DAY_6}, #{const DAY_7} ]

abDayItems :: [CInt]
abDayItems =
  [ #{const ABDAY_1}, #{const ABDAY_2}, #{const ABDAY_3}, #{const ABDAY_4}
  , #{const ABDAY_5}, #{const ABDAY_6}, #{const ABDAY_7} ]

itemAM, itemPM, itemDFmt, itemTFmt, itemDTFmt :: CInt
itemAM    = #{const AM_STR}
itemPM    = #{const PM_STR}
itemDFmt  = #{const D_FMT}
itemTFmt  = #{const T_FMT}
itemDTFmt = #{const D_T_FMT}

-- | Query one @nl_item@ from a locale and decode it as UTF-8 (leniently).  The result is copied into a Haskell 'String'
--   immediately, so it remains valid after the locale is freed.
peekItem :: CLocale -> CInt -> IO String
peekItem loc item = do
  cs <- c_nl_langinfo_l item loc
  if cs == nullPtr
    then return ""
    else T.unpack . TE.decodeUtf8With TEE.lenientDecode <$> B.packCString cs

buildLocale :: String -> CLocale -> IO Locale
buildLocale lid loc = do
  mons  <- mapM (peekItem loc) monthItems
  amons <- mapM (peekItem loc) abMonthItems
  days  <- mapM (peekItem loc) dayItems
  adays <- mapM (peekItem loc) abDayItems
  am    <- peekItem loc itemAM
  pm    <- peekItem loc itemPM
  df    <- peekItem loc itemDFmt
  tf    <- peekItem loc itemTFmt
  dtf   <- peekItem loc itemDTFmt
  return Locale
    { localeId          = lid
    , monthNames        = mons
    , monthNamesShort   = amons
    , dayNames          = days
    , dayNamesShort     = adays
    , amName            = am
    , pmName            = pm
    , rawDateFormat     = df
    , rawTimeFormat     = tf
    , rawDateTimeFormat = dtf
    }

-- | Create a fresh @locale_t@ for the given name, run the action, and always free the handle.  Returns 'Nothing' when
--   the name does not resolve to an installed locale.
withNewLocale :: String -> (CLocale -> IO a) -> IO (Maybe a)
withNewLocale name act =
  withCString name $ \cname ->
    bracket (c_newlocale lcAllMask cname nullPtr) freeIfNonNull $ \loc ->
      if loc == nullPtr then return Nothing else Just <$> act loc
  where
    freeIfNonNull l = when (l /= nullPtr) (c_freelocale l)

-- | Read a specific locale by name (e.g. @\"de_DE.UTF-8\"@).  'Nothing' if it is not installed on the machine.
loadLocaleByName :: String -> IO (Maybe Locale)
loadLocaleByName name = withNewLocale name (buildLocale name)

-- | Read the process's current locale, as selected by the environment (@LC_ALL@ \/ @LC_TIME@ \/ @LANG@), falling back
--   to the POSIX @C@ locale.
loadCurrentLocale :: IO Locale
loadCurrentLocale = do
  lid <- currentLocaleId
  m <- withNewLocale "" (buildLocale lid)
  case m of
    Just l  -> return l
    Nothing -> loadLocaleByName "C" >>= maybe (ioError (userError "loadCurrentLocale: could not load the POSIX C locale")) return

-- | The identifier of the current locale, taken from the first of @LC_ALL@, @LC_TIME@ or @LANG@ that is set to a
--   non-empty value, defaulting to @\"C\"@.
currentLocaleId :: IO String
currentLocaleId = do
  vals <- mapM lookupEnv ["LC_ALL", "LC_TIME", "LANG"]
  return $ case filter (not . null) (catMaybes vals) of
    (x:_) -> x
    []    -> "C"
