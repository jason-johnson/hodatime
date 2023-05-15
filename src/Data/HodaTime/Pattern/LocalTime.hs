module Data.HodaTime.Pattern.LocalTime
(
  -- * Standard Patterns
  time
  -- * Custom Patterns
  --
  -- | Used to create specialized patterns
  ,pHH
  ,phh
  ,hour'
  ,pmm
  ,minute'
  ,pss
  ,second'
)
where

import Data.HodaTime.Pattern.Internal
import Data.HodaTime.Pattern.ParseTypes (TimeInfo)
import qualified Data.HodaTime.Pattern.ParseTypes as PT(hour, minute, second)
import Data.HodaTime.LocalTime.Internal (HasLocalTime)
import qualified Data.HodaTime.LocalTime.Internal as LT(hour, minute, second)
import Control.Applicative ((<|>))
import Text.Parsec (oneOf, digit)
import qualified Text.Parsec as P (char)

-- x = maybe (error "duh") id $ localTime 1 2 3 0
-- pat = pHH <% char ':' <> pmm <% char ':' <> pss
-- parse pat "01:01:01" :: IO LocalTime
-- format pat x

-- | The double digit hour of day in the 12-hour clock; a value 1-12. When parsing, if no am/pm designator is specified, the parsed value is in the morning.
phh :: HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
phh = pat_lens LT.hour (p_a <|> p_b) f_shown_two "hour: 01-12"
  where
    p_a = digitsToInt <$> P.char '0' <*> oneOf ['1'..'9']
    p_b = digitsToInt <$> P.char '1' <*> oneOf ['0'..'2']

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

time ::  HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
time = pHH <% char ':' <> pmm <% char ':' <> pss