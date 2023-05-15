module Data.HodaTime.Pattern.LocalTime
(
  -- TODO: We don't expose these, they are building blocks
   pat_hour
  ,pat_hour'
  ,pat_hour_12
  ,pat_minute
  ,pat_minute'
  ,pat_second
  ,pat_second'
  -- * Patterns
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
-- pat = pat_hour <% pat_char ':' <> pat_minute <% pat_char ':' <> pat_second
-- parse pat "01:01:01" :: IO LocalTime
-- format pat x

pat_hour :: HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
pat_hour = pat_lens LT.hour (p_a <|> p_b) f_shown_two "hour: 00-23"
  where
    p_a = digitsToInt <$> oneOf ['0', '1'] <*> digit 
    p_b = digitsToInt <$> P.char '2' <*> oneOf ['0'..'3']

pat_hour' :: HasLocalTime lt => Pattern (TimeInfo -> TimeInfo) (lt -> String) String
pat_hour' = pat_lens' PT.hour LT.hour (p_a <|> p_b) f_shown_two "hour: 00-23"
  where
    p_a = digitsToInt <$> oneOf ['0', '1'] <*> digit 
    p_b = digitsToInt <$> P.char '2' <*> oneOf ['0'..'3']

pat_hour_12 :: HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
pat_hour_12 = pat_lens LT.hour (p_a <|> p_b) f_shown_two "hour: 00-12"
  where
    p_a = digitsToInt <$> P.char '0' <*> digit
    p_b = digitsToInt <$> P.char '1' <*> oneOf ['0'..'2']

pat_minute :: HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
pat_minute = pat_lens LT.minute p_sixty f_shown_two "minute: 00-59"

pat_minute' :: HasLocalTime lt => Pattern (TimeInfo -> TimeInfo) (lt -> String) String
pat_minute' = pat_lens' PT.minute LT.minute p_sixty f_shown_two "minute: 00-59"

pat_second :: HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
pat_second = pat_lens LT.second p_sixty f_shown_two "second: 00-59"

pat_second' :: HasLocalTime lt => Pattern (TimeInfo -> TimeInfo) (lt -> String) String
pat_second' = pat_lens' PT.second LT.second p_sixty f_shown_two "second: 00-59"