module Data.HodaTime.Pattern.LocalTime
(
  -- * Standard Patterns
   pt
  ,pT
  -- * Custom Patterns
  --
  -- | Used to create specialized patterns
  ,pHH
  ,phh
  ,pmm
  ,pss
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
import qualified Data.HodaTime.LocalTime.Internal as LT(hour, minute, second)
import Data.HodaTime.Internal.Lens (view, set)
import Control.Applicative ((<|>))
import Formatting (Format, later)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TLB
import Text.Parsec (oneOf, digit, (<?>))
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

-- | The double digit hour of day in the 24-hour clock; a value 00-23.
pHH :: HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
pHH = pat_lens LT.hour (p_a <|> p_b) f_shown_two "hour: 00-23"
  where
    p_a = digitsToInt <$> oneOf ['0', '1'] <*> digit 
    p_b = digitsToInt <$> P.char '2' <*> oneOf ['0'..'3']

hour' :: HasLocalTime lt => Pattern (TimeInfo -> TimeInfo) (lt -> String) String
hour' = pat_lens' PT.hour LT.hour (p_a <|> p_b) f_shown_two "hour: 00-23"
  where
    p_a = digitsToInt <$> oneOf ['0', '1'] <*> digit 
    p_b = digitsToInt <$> P.char '2' <*> oneOf ['0'..'3']

-- | The double digit minute of day in the 24-hour clock; a value 00-59.
pmm :: HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
pmm = pat_lens LT.minute p_sixty f_shown_two "minute: 00-59"

minute' :: HasLocalTime lt => Pattern (TimeInfo -> TimeInfo) (lt -> String) String
minute' = pat_lens' PT.minute LT.minute p_sixty f_shown_two "minute: 00-59"

-- | The double digit second of day in the 24-hour clock; a value 00-59.
pss :: HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
pss = pat_lens LT.second p_sixty f_shown_two "second: 00-59"

second' :: HasLocalTime lt => Pattern (TimeInfo -> TimeInfo) (lt -> String) String
second' = pat_lens' PT.second LT.second p_sixty f_shown_two "second: 00-59"

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