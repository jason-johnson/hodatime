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
  ,pminute
  ,pmm
  ,psecond
  ,pss
  ,pfrac
  ,pp
  ,ppp
  ,hour'
  ,minute'
  ,second'
)
where

import Data.HodaTime.Pattern.Internal
import Data.HodaTime.Pattern.ParseTypes (TimeInfo)
import qualified Data.HodaTime.Pattern.ParseTypes as PT(hour, minute, second)
import Data.HodaTime.LocalTime.Internal (HasLocalTime)
import qualified Data.HodaTime.LocalTime.Internal as LT(hour, minute, second, nanosecond)
import Data.HodaTime.Internal.Lens (view, set)
import Control.Applicative ((<|>))
import Formatting (Format, later, left, (%.))
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TLB
import Text.Parsec (oneOf, digit, count, (<?>))
import qualified Text.Parsec as P (char)

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
phh = Pattern par fmt
  where
    par = (adjust <$> (p_a <|> p_b)) <?> "hour: 01-12"
    p_a = digitsToInt <$> P.char '0' <*> oneOf ['1'..'9']
    p_b = digitsToInt <$> P.char '1' <*> oneOf ['0'..'2']
    adjust n lt = set LT.hour (12 * (view LT.hour lt `div` 12) + (n `mod` 12)) lt   -- NOTE: replace only the 1-12 position, preserve the AM/PM half
    fmt = f_shown_two (to12 . view LT.hour)
    to12 h = if h' == 0 then 12 else h' where h' = h `mod` 12

-- | The hour of day in the 24-hour clock as @w@ digits, zero-padded; a width of @1@ means /no padding/.  Values 00-23.
phour :: HasLocalTime lt => Int -> Pattern (lt -> lt) (lt -> String) String
phour w = pat_lens LT.hour (pDigits w 2 0 23) (f_shown_pad w) "hour: 00-23"

-- | The double digit hour of day in the 24-hour clock (@'phour' 2@); a value 00-23.
pHH :: HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
pHH = phour 2

hour' :: HasLocalTime lt => Pattern (TimeInfo -> TimeInfo) (lt -> String) String
hour' = pat_lens' PT.hour LT.hour (p_a <|> p_b) f_shown_two "hour: 00-23"
  where
    p_a = digitsToInt <$> oneOf ['0', '1'] <*> digit 
    p_b = digitsToInt <$> P.char '2' <*> oneOf ['0'..'3']

-- | The minute of the hour as @w@ digits, zero-padded; a width of @1@ means /no padding/.  Values 00-59.
pminute :: HasLocalTime lt => Int -> Pattern (lt -> lt) (lt -> String) String
pminute w = pat_lens LT.minute (pDigits w 2 0 59) (f_shown_pad w) "minute: 00-59"

-- | The double digit minute of the hour (@'pminute' 2@); a value 00-59.
pmm :: HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
pmm = pminute 2

minute' :: HasLocalTime lt => Pattern (TimeInfo -> TimeInfo) (lt -> String) String
minute' = pat_lens' PT.minute LT.minute p_sixty f_shown_two "minute: 00-59"

-- | The second of the minute as @w@ digits, zero-padded; a width of @1@ means /no padding/.  Values 00-59.
psecond :: HasLocalTime lt => Int -> Pattern (lt -> lt) (lt -> String) String
psecond w = pat_lens LT.second (pDigits w 2 0 59) (f_shown_pad w) "second: 00-59"

-- | The double digit second of the minute (@'psecond' 2@); a value 00-59.
pss :: HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
pss = psecond 2

second' :: HasLocalTime lt => Pattern (TimeInfo -> TimeInfo) (lt -> String) String
second' = pat_lens' PT.second LT.second p_sixty f_shown_two "second: 00-59"

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
    par = ((set LT.nanosecond . (* scale) . read) <$> count n digit) <?> ("fractional second: " ++ show n ++ " digits")
    fmt = left n '0' %. f_shown (\lt -> view LT.nanosecond lt `div` scale)

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

-- | Rewrite only the AM\/PM half of the hour (morning \<-\> afternoon), preserving the 1-12 position set by 'phh'.
amPmSetter :: HasLocalTime lt => Bool -> lt -> lt
amPmSetter isPM lt = set LT.hour (h12 + if isPM then 12 else 0) lt
  where h12 = view LT.hour lt `mod` 12

-- | Render the AM\/PM designator, choosing the form via the supplied function (True == PM).
amPmFormat :: HasLocalTime lt => (Bool -> String) -> Format String (lt -> String)
amPmFormat render = later (TLB.fromText . T.pack . render . (>= (12 :: Int)) . view LT.hour)

-- | Short format pattern. Currently defined as "HH:mm" but should eventually follow the locale
pt :: HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
pt = pHH <% char ':' <> pmm

-- | Long format pattern. Currently defined as "HH:mm:ss" but should eventually follow locale
pT ::  HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
pT = pHH <% char ':' <> pmm <% char ':' <> pss
-- | The round-trippable time pattern, "HH:mm:ss.fffffffff" (nanosecond precision).
pr ::  HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
pr = pT <% char '.' <> pfrac 9