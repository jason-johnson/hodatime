{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.HodaTime.Pattern.CalendarDate
(
  -- TODO: We don't expose these, they are building blocks
   pat_day
  ,pat_month
  ,pat_year
)
where

import Data.HodaTime.Pattern.Internal
import Data.HodaTime.CalendarDateTime.Internal (HasDate, day, month, monthl, year, Month, CalendarDate, IsCalendar)
import qualified  Data.Text as T
import qualified  Data.Text.Lazy.Builder as TLB
import Text.Parsec (digit, count, oneOf, string, choice)
import Formatting (left, (%.), mapf, later)
import Data.HodaTime.Internal.Lens (view, set, Lens)

-- d1 = maybe (error "duh") id $ calendarDate 1 January 2000
-- d2 = maybe (error "duh") id $ calendarDate 3 March 2020
-- pat = pat_year 4 <% pat_char '/'

pat_year :: HasDate d => Int -> Pattern (d -> d) (d -> String) String
pat_year c = pat_lens year p fmt $ "year: " ++ zeros ++ "-" ++ nines
  where
    rep = replicate c
    zeros = rep '0'
    nines = rep '9'
    p = read <$> count c digit 
    fmt x = left c '0' %. f_shown x

pat_month :: forall cal. (Bounded (Month cal), Read (Month cal), Show (Month cal), Enum (Month cal), IsCalendar cal) => Pattern (CalendarDate cal -> CalendarDate cal) (CalendarDate cal -> String) String
pat_month = Pattern p fmt
  where
    months' = [minBound..maxBound] :: [Month cal]
    months = choice . fmap (string . show) $ months'
    p = set monthl <$> p'
    p' = (fromEnum :: Month cal -> Int) . read <$> months
    fmt = fmt' $ view monthl
    fmt' x = later (TLB.fromText . T.pack . show . (toEnum :: Int -> Month cal) . x)

pat_day :: HasDate d => Pattern (d -> d) (d -> String) String
pat_day = pat_lens day p_sixty f_shown_two "second: 00-59"