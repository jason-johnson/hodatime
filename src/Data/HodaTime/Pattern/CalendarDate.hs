{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Data.HodaTime.Pattern.CalendarDate
(
  -- * Standard Patterns
   pd
  ,pD
  -- * Custom Patterns
  --
  -- | Used to create specialized patterns
  -- * Patterns
  ,pyyyy
  ,pMM
  ,pMMMM
  ,pdd
)
where

import Data.HodaTime.Pattern.Internal
import Data.HodaTime.CalendarDateTime.Internal (HasDate, Month, IsCalendar, monthl)
import qualified Data.HodaTime.CalendarDateTime.Internal as CDT (day, year)
import qualified  Data.Text as T
import qualified  Data.Text.Lazy.Builder as TLB
import Data.Char(toLower, toUpper)
import Control.Applicative ((<|>))
import Text.Parsec (digit, count, choice, oneOf, try)
import qualified Text.Parsec as P (char)
import Formatting (left, (%.), later)

-- d1 = maybe (error "duh") id $ calendarDate 1 January 2000
-- d2 = maybe (error "duh") id $ calendarDate 3 March 2020
-- format Data.HodaTime.Pattern.CalendarDate.date d1
-- format Data.HodaTime.Pattern.CalendarDate.date d2
-- parse Data.HodaTime.Pattern.CalendarDate.date "2000/March/01" :: IO (CalendarDate Gregorian)

-- | Absolute year in exactly 4 digits; values 0000-9999 (note: not all dates will be valid in all calendars, if the date is too early it will clamp to earliest valid date)
pyyyy :: HasDate d => Pattern (d -> d) (d -> String) String
pyyyy = pat_lens CDT.year p fmt "year: 0000-9999"
  where
    p = read <$> count 4 digit
    fmt x = left 4 '0' %. f_shown x

pMM :: HasDate d => Pattern (d -> d) (d -> String) String
pMM = pat_lens monthl p fmt "month: 01-12"
  where
    p = pred <$> p_a <|> p_b
    p_a = digitsToInt <$> P.char '0' <*> digit
    p_b = digitsToInt <$> P.char '1' <*> oneOf ['0'..'2']
    fmt x = left 2 '0' %. later (TLB.fromText . T.pack . show . succ . x)

-- | Full month name, parsed case-insensitively.  Formats in title case
pMMMM :: forall cal d c. (d ~ c cal, IsCalendar cal, HasDate d, Bounded (Month cal), Read (Month cal), Show (Month cal), Enum (Month cal)) => Pattern (d -> d) (d -> String) String
pMMMM = pat_lens monthl p' fmt' $ "month: " ++ show fm ++ "-" ++ show lm
  where
    caseInsensitiveChar c = do
      _ <- P.char (toLower c) <|> P.char (toUpper c)
      return c
    caseInsensitiveString = mapM caseInsensitiveChar
    fm = minBound :: Month cal
    lm = maxBound :: Month cal
    months = choice . fmap (try . caseInsensitiveString . show) $ [fm..lm]
    p' = (fromEnum :: Month cal -> Int) . read <$> months
    fmt' x = later (TLB.fromText . T.pack . show . (toEnum :: Int -> Month cal) . x)

-- | Day of month - zero-padded
pdd :: HasDate d => Pattern (d -> d) (d -> String) String
pdd = pat_lens CDT.day (p_a <|> p_b) f_shown_two "day: 01-31"
  where
    p_a = digitsToInt <$> oneOf ['0'..'2'] <*> digit
    p_b = digitsToInt <$> P.char '3' <*> oneOf ['0', '1']

-- | This is the short date pattern, currently defined as "dd/MM/yyyy".
pd :: (HasDate (c cal), IsCalendar cal, Bounded (Month cal), Read (Month cal), Show (Month cal), Enum (Month cal)) => Pattern (c cal -> c cal) (c cal -> String) String
pd = pdd <% char '/' <> pMM <% char '/' <> pyyyy

-- | This is the long date pattern, currently defined as "dddd, dd MMMM yyyy".
pD :: (HasDate (c cal), IsCalendar cal, Bounded (Month cal), Read (Month cal), Show (Month cal), Enum (Month cal)) => Pattern (c cal -> c cal) (c cal -> String) String
pD = pdd <% char ' ' <> pMMMM <% char ' ' <> pyyyy