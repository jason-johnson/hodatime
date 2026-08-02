-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Pattern.LocalTime
-- Copyright   :  (C) 2017 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- Patterns for a 'Data.HodaTime.LocalTime.LocalTime': the standard time layouts (@pt@, @pT@, @pr@) together with the
-- individual field patterns (@pHH@, @phh@, @pmm@, @pss@, @pfrac@, @ppp@ and friends) from which custom time patterns are
-- built.  The primed variant @ppp'@ takes a 'Data.HodaTime.Locale.Locale' for its AM\/PM designators.
----------------------------------------------------------------------------
module Data.HodaTime.Pattern.LocalTime
(
  -- * Standard Patterns
   pt
  ,pT
  ,pr
  -- * Custom Patterns
  --
  -- | Used to create specialized patterns
  ,phour
  ,pHH
  ,phh
  ,phhSpace
  ,pminute
  ,pmm
  ,psecond
  ,pss
  ,pfrac
  ,pp
  ,ppp
  ,ppp'
  ,pPeriod
  ,hour'
  ,minute'
  ,second'
)
where

import Data.HodaTime.Pattern.Internal
import Data.HodaTime.Pattern.ParseTypes (TimeInfo)
import qualified Data.HodaTime.Pattern.ParseTypes as PT(hour, minute, second)
import Data.HodaTime.LocalTime.Internal (HasLocalTime)
import qualified Data.HodaTime.LocalTime.Internal as LT(hour, setHour, minute, setMinute, second, setSecond, nanosecond, setNanosecond)
import Control.Applicative ((<|>))
import Formatting (Format, later, left, (%.))
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TLB
import Text.Parsec (oneOf, digit, count, try, (<?>))
import qualified Text.Parsec as P (char)
import Data.HodaTime.Locale.Internal (Locale(..))

-- x = maybe (error "duh") id $ localTime 1 2 3 0
-- parse pT "01:01:01" :: IO LocalTime
-- format pT x
-- format pt x

-- | The double digit hour of day in the 12-hour clock; a value 01-12.  When formatting, the underlying 24-hour
--   value is folded into the 1-12 range (e.g. both 00:00 and 12:00 render as @12@).  When parsing, the value is
--   combined with an AM\/PM designator ('pp' \/ 'ppp') if one is present in the pattern; if no designator is
--   present the value is interpreted as the morning (so @12@ parses to midnight).
--
--   'phh' and the AM\/PM designators are /order independent/: each only rewrites its own portion of the hour, so
--   @'phh' '<%' 'char' \' \' '<>' 'ppp'@ and @'ppp' '<%' 'char' \' \' '<>' 'phh'@ both round-trip correctly.
phh :: HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
phh = twelveHour paddedNum f_shown_two
  where
    paddedNum = (digitsToInt <$> P.char '0' <*> oneOf ['1'..'9']) <|> (digitsToInt <$> P.char '1' <*> oneOf ['0'..'2'])

-- | The hour of day in the 12-hour clock, /space/-padded to two characters (the @strftime@ @%l@ convention), e.g.
--   @\" 3\"@.  Like 'phh' it folds the 24-hour value into 1-12 and combines with an AM\/PM designator on parse.
phhSpace :: HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
phhSpace = twelveHour (pDigitsSpace 2 1 12) (f_shown_spad 2)

-- | Shared builder for the 12-hour clock hour: @numP@ parses the 1-12 value and @mkFmt@ renders it (given a getter of
--   the folded 1-12 hour).  Only the 1-12 position of the hour is rewritten on parse, preserving the AM\/PM half so it
--   stays order-independent with 'pp'\/'ppp'.
twelveHour :: HasLocalTime lt => Parser Int String -> ((lt -> Int) -> Format String (lt -> String)) -> Pattern (lt -> lt) (lt -> String) String
twelveHour numP mkFmt = Pattern par (mkFmt (to12 . LT.hour))
  where
    par = (adjust <$> numP) <?> "hour: 01-12"
    adjust n lt = LT.setHour (12 * (LT.hour lt `div` 12) + (n `mod` 12)) lt
    to12 h = if h' == 0 then 12 else h' where h' = h `mod` 12

-- | The hour of day in the 24-hour clock as @w@ digits, zero-padded; a width of @1@ means /no padding/.  Values 00-23.
phour :: HasLocalTime lt => Int -> Pattern (lt -> lt) (lt -> String) String
phour w = pat_field LT.hour LT.setHour (pDigits w 2 0 23) (f_shown_pad w) "hour: 00-23"

-- | The double digit hour of day in the 24-hour clock (@'phour' 2@); a value 00-23.
pHH :: HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
pHH = phour 2

hour' :: HasLocalTime lt => Pattern (TimeInfo -> TimeInfo) (lt -> String) String
hour' = pat_lens_field PT.hour LT.hour (p_a <|> p_b) f_shown_two "hour: 00-23"
  where
    p_a = digitsToInt <$> oneOf ['0', '1'] <*> digit 
    p_b = digitsToInt <$> P.char '2' <*> oneOf ['0'..'3']

-- | The minute of the hour as @w@ digits, zero-padded; a width of @1@ means /no padding/.  Values 00-59.
pminute :: HasLocalTime lt => Int -> Pattern (lt -> lt) (lt -> String) String
pminute w = pat_field LT.minute LT.setMinute (pDigits w 2 0 59) (f_shown_pad w) "minute: 00-59"

-- | The double digit minute of the hour (@'pminute' 2@); a value 00-59.
pmm :: HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
pmm = pminute 2

minute' :: HasLocalTime lt => Pattern (TimeInfo -> TimeInfo) (lt -> String) String
minute' = pat_lens_field PT.minute LT.minute p_sixty f_shown_two "minute: 00-59"

-- | The second of the minute as @w@ digits, zero-padded; a width of @1@ means /no padding/.  Values 00-59.
psecond :: HasLocalTime lt => Int -> Pattern (lt -> lt) (lt -> String) String
psecond w = pat_field LT.second LT.setSecond (pDigits w 2 0 59) (f_shown_pad w) "second: 00-59"

-- | The double digit second of the minute (@'psecond' 2@); a value 00-59.
pss :: HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
pss = psecond 2

second' :: HasLocalTime lt => Pattern (TimeInfo -> TimeInfo) (lt -> String) String
second' = pat_lens_field PT.second LT.second p_sixty f_shown_two "second: 00-59"

-- | Fractional seconds of a fixed width @n@ (1-9 digits, since the underlying resolution is nanoseconds).  Formatting
--   shows exactly @n@ zero-padded digits (dropping any finer resolution); parsing reads exactly @n@ digits and scales
--   them back up to nanoseconds (so @'pfrac' 3@ parses milliseconds).  A width outside 1-9 is a programmer error.
--
--   NOTE: this is the first pattern that takes a parameter (all the others are nullary values).  The parameterized form
--   was chosen because @pf@\/@pF@ are already taken by the standard date\/time patterns and because it covers every
--   width uniformly.  If parameterized patterns stay rare we may revisit this; a trailing-zero-trimming variant (the
--   NodaTime @F@ specifier) could also be added later.
pfrac :: HasLocalTime lt => Int -> Pattern (lt -> lt) (lt -> String) String
pfrac n
  | n < 1 || n > 9 = error "pfrac: fractional second width must be between 1 and 9"
  | otherwise = Pattern par fmt
  where
    scale = 10 ^ (9 - n) :: Int
    par = ((LT.setNanosecond . (* scale) . read) <$> count n digit) <?> ("fractional second: " ++ show n ++ " digits")
    fmt = left n '0' %. f_shown (\lt -> LT.nanosecond lt `div` scale)

-- | 12 hour clock time period designation short form; either @A@ or @P@.  See 'ppp' for the long form.
pp ::  HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
pp = Pattern par (amPmFormat render)
  where
    par = (amPmSetter <$> desig) <?> "period: A or P"
    desig = (True <$ oneOf "Pp") <|> (False <$ oneOf "Aa")
    render isPM = if isPM then "P" else "A"

-- | 12 hour clock time period designation full form; either @AM@ or @PM@.  When parsing, this only sets whether the
--   hour is in the morning or afternoon; combine with 'phh' to parse a full 12-hour time.
ppp ::  HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
ppp = Pattern par (amPmFormat render)
  where
    par = (amPmSetter <$> desig) <?> "period: AM or PM"
    desig = (\c -> c `elem` "Pp") <$> oneOf "AaPp" <* oneOf "Mm"
    render isPM = if isPM then "PM" else "AM"

-- | 12 hour clock time period designator using an explicit @(am, pm)@ pair, instead of the built-in @AM@\/@PM@ literals.
--   This is the calendar-agnostic core behind the locale-aware 'ppp''.  Parsing matches either designator
--   case-insensitively (PM tried first); as with 'ppp' it only sets morning\/afternoon, so combine it with 'phh'.
pPeriod :: HasLocalTime lt => (String, String) -> Pattern (lt -> lt) (lt -> String) String
pPeriod (am, pm) = Pattern par (amPmFormat render)
  where
    par = (amPmSetter <$> desig) <?> "period"
    desig = (True <$ try (caseInsensitiveString pm)) <|> (False <$ try (caseInsensitiveString am))
    render isPM = if isPM then pm else am

-- | 12 hour clock time period designator in the given 'Locale' (e.g. the POSIX @AM_STR@\/@PM_STR@); the locale-aware
--   counterpart to 'ppp'.  NOTE: some locales (e.g. German) leave these designators empty, in which case this pattern
--   cannot round-trip; prefer a 24-hour pattern there.
ppp' :: HasLocalTime lt => Locale -> Pattern (lt -> lt) (lt -> String) String
ppp' loc = pPeriod (amName loc, pmName loc)

-- | Rewrite only the AM\/PM half of the hour (morning \<-\> afternoon), preserving the 1-12 position set by 'phh'.
amPmSetter :: HasLocalTime lt => Bool -> lt -> lt
amPmSetter isPM lt = LT.setHour (h12 + if isPM then 12 else 0) lt
  where h12 = LT.hour lt `mod` 12

-- | Render the AM\/PM designator, choosing the form via the supplied function (True == PM).
amPmFormat :: HasLocalTime lt => (Bool -> String) -> Format String (lt -> String)
amPmFormat render = later (TLB.fromText . T.pack . render . (>= (12 :: Int)) . LT.hour)

-- | Short format pattern. Currently defined as "HH:mm" but should eventually follow the locale
pt :: HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
pt = pHH <% char ':' <> pmm

-- | Long format pattern. Currently defined as "HH:mm:ss" but should eventually follow locale
pT ::  HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
pT = pHH <% char ':' <> pmm <% char ':' <> pss
-- | The round-trippable time pattern, "HH:mm:ss.fffffffff" (nanosecond precision).
pr ::  HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
pr = pT <% char '.' <> pfrac 9