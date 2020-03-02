module Data.HodaTime.Pattern.LocalTime
(
  -- TODO: We don't expose these, they are building blocks
   pat_hour
  ,pat_hour_12
  ,pat_minute
  ,pat_second
)
where

import Data.HodaTime.Pattern.Internal
import Data.HodaTime.LocalTime.Internal (HasLocalTime, hour, minute, second)
import Control.Applicative ((<|>))
import Text.Parsec (char, oneOf, digit)

pat_hour :: HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
pat_hour = pat_lens hour (p_a <|> p_b) f_shown_two "hour: 00-24"
  where
    p_a = digitsToInt <$> oneOf ['0', '1'] <*> digit 
    p_b = digitsToInt <$> char '2' <*> oneOf ['0'..'3']

pat_hour_12 :: HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
pat_hour_12 = pat_lens hour (p_a <|> p_b) f_shown_two "hour: 00-12"
  where
    p_a = digitsToInt <$> char '0' <*> digit
    p_b = digitsToInt <$> char '1' <*> oneOf ['0'..'2']

pat_minute :: HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
pat_minute = pat_lens minute p_sixty f_shown_two "minute: 00-59"

pat_second :: HasLocalTime lt => Pattern (lt -> lt) (lt -> String) String
pat_second = pat_lens second p_sixty f_shown_two "second: 00-59"