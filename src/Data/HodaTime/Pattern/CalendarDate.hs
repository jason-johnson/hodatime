{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Data.HodaTime.Pattern.CalendarDate
(
  -- TODO: We don't expose these, they are building blocks
   pat_day
  ,pat_month
  ,pat_year
  -- * Patterns
)
where

import Data.HodaTime.Pattern.Internal
import Data.HodaTime.CalendarDateTime.Internal (HasDate, day, monthl, year, Month, IsCalendar)
import qualified  Data.Text as T
import qualified  Data.Text.Lazy.Builder as TLB
import Control.Applicative ((<|>))
import Text.Parsec (digit, count, choice, oneOf, try)
import qualified Text.Parsec as P (char, string)
import Formatting (left, (%.), later)

-- d1 = maybe (error "duh") id $ calendarDate 1 January 2000
-- d2 = maybe (error "duh") id $ calendarDate 3 March 2020
-- pat = pat_year 4 <% pat_char '/'
-- parse' (pat_year 4 <% pat_char '/' <> pat_month <% pat_char '/' <> pat_day <% pat_char ' ' <> pat_hour <% pat_char ':' <> pat_minute <% pat_char ':' <> pat_second) "2000/March/01 01:01:01" dt

pat_year :: HasDate d => Int -> Pattern (d -> d) (d -> String) String
pat_year c = pat_lens year p fmt $ "year: " ++ zeros ++ "-" ++ nines
  where
    rep = replicate c
    zeros = rep '0'
    nines = rep '9'
    p = read <$> count c digit 
    fmt x = left c '0' %. f_shown x

pat_month :: forall cal d c. (d ~ c cal, IsCalendar cal, HasDate d, Bounded (Month cal), Read (Month cal), Show (Month cal), Enum (Month cal)) => Pattern (d -> d) (d -> String) String
pat_month = pat_lens monthl p' fmt' $ "month: " ++ show fm ++ "-" ++ show lm
  where
    fm = minBound :: Month cal
    lm = maxBound :: Month cal
    months = choice . fmap (try . P.string . show) $ [fm..lm]
    p' = (fromEnum :: Month cal -> Int) . read <$> months
    fmt' x = later (TLB.fromText . T.pack . show . (toEnum :: Int -> Month cal) . x)

pat_day :: HasDate d => Pattern (d -> d) (d -> String) String
pat_day = pat_lens day (p_a <|> p_b) f_shown_two "day: 01-31"
  where
    p_a = digitsToInt <$> oneOf ['0'..'2'] <*> digit
    p_b = digitsToInt <$> P.char '3' <*> oneOf ['0', '1']