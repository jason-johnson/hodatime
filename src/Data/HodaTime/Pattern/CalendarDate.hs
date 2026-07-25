{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Data.HodaTime.Pattern.CalendarDate
(
  -- * Standard Patterns
   pd
  ,pD
  ,pR
  -- * Custom Patterns
  --
  -- | Used to create specialized patterns.
  ,pyyyy
  ,pMM
  ,pMMM
  ,pMMMM
  ,pdd
  ,pddd
  ,pdddd
)
where

import Data.HodaTime.Pattern.Internal
import Data.HodaTime.CalendarDateTime.Internal (HasDate, Month, IsCalendar, monthl, dayOfWeek, DoW)
import qualified Data.HodaTime.CalendarDateTime.Internal as CDT (day, year)
import qualified  Data.Text as T
import qualified  Data.Text.Lazy.Builder as TLB
import Data.Char(toLower, toUpper)
import Control.Applicative ((<|>))
import Text.Parsec (digit, count, choice, oneOf, try, Parsec)
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

-- | Month of year specified as a number - zero-padded
pMM :: HasDate d => Pattern (d -> d) (d -> String) String
pMM = pat_lens monthl p fmt "month: 01-12"
  where
    p = pred <$> (p_a <|> p_b)
    p_a = digitsToInt <$> P.char '0' <*> digit
    p_b = digitsToInt <$> P.char '1' <*> oneOf ['0'..'2']
    fmt x = left 2 '0' %. later (TLB.fromText . T.pack . show . succ . x)

-- | Full month name, parsed case-insensitively.  Formats in title case
pMMMM :: forall cal d c. (d ~ c cal, IsCalendar cal, HasDate d, Bounded (Month cal), Read (Month cal), Show (Month cal), Enum (Month cal)) => Pattern (d -> d) (d -> String) String
pMMMM = pat_lens monthl p' fmt' $ "month: " ++ show fm ++ "-" ++ show lm
  where
    fm = minBound :: Month cal
    lm = maxBound :: Month cal
    months = choice . fmap (try . caseInsensitiveString . show) $ [fm..lm]
    p' = (fromEnum :: Month cal -> Int) . read <$> months
    fmt' x = later (TLB.fromText . T.pack . show . (toEnum :: Int -> Month cal) . x)
-- | Abbreviated month name (e.g. @Jan@), parsed case-insensitively and formatted in title case.
--
--   NOTE: the abbreviation is simply the first three letters of the month name, so in calendars where two months share
--   a three-letter prefix (e.g. the Hebrew @AdarI@ and @Adar@) parsing is ambiguous and resolves to the first match in
--   month order.  Use 'pMMMM' (full name) or 'pMM' (number) when you need an unambiguous round-trip.
pMMM :: forall cal d c. (d ~ c cal, IsCalendar cal, HasDate d, Bounded (Month cal), Show (Month cal), Enum (Month cal)) => Pattern (d -> d) (d -> String) String
pMMM = pat_lens monthl p' fmt' $ "month: " ++ abbr fm ++ "-" ++ abbr lm
  where
    fm = minBound :: Month cal
    lm = maxBound :: Month cal
    abbr = take 3 . show
    p' = choice . fmap (\m -> fromEnum m <$ try (caseInsensitiveString (abbr m))) $ [fm..lm]
    fmt' x = later (TLB.fromText . T.pack . abbr . (toEnum :: Int -> Month cal) . x)
-- | Day of month - zero-padded
pdd :: HasDate d => Pattern (d -> d) (d -> String) String
pdd = pat_lens CDT.day (p_a <|> p_b) f_shown_two "day: 01-31"
  where
    p_a = digitsToInt <$> oneOf ['0'..'2'] <*> digit
    p_b = digitsToInt <$> P.char '3' <*> oneOf ['0', '1']

-- | Abbreviated day of week name (e.g. @Mon@), parsed case-insensitively and formatted in title case.  Note: on parse
--   this only /consumes/ the weekday, it is not validated against the day\/month\/year (which fully determine the date).
pddd :: forall d. (HasDate d, Show (DoW d), Enum (DoW d), Bounded (DoW d)) => Pattern (d -> d) (d -> String) String
pddd = Pattern par fmt
  where
    names = [minBound .. maxBound] :: [DoW d]
    abbr = take 3 . show
    par = id <$ (choice . fmap (try . caseInsensitiveString . abbr) $ names)
    fmt = later (TLB.fromText . T.pack . abbr . dayOfWeek)

-- | Full day of week name (e.g. @Monday@), parsed case-insensitively and formatted in title case.  Note: on parse this
--   only /consumes/ the weekday, it is not validated against the day\/month\/year (which fully determine the date).
pdddd :: forall d. (HasDate d, Show (DoW d), Enum (DoW d), Bounded (DoW d)) => Pattern (d -> d) (d -> String) String
pdddd = Pattern par fmt
  where
    names = [minBound .. maxBound] :: [DoW d]
    par = id <$ (choice . fmap (try . caseInsensitiveString . show) $ names)
    fmt = later (TLB.fromText . T.pack . show . dayOfWeek)

-- | This is the short date pattern, currently defined as "dd/MM/yyyy".
pd :: HasDate d => Pattern (d -> d) (d -> String) String
pd = pdd <% char '/' <> pMM <% char '/' <> pyyyy

-- | This is the long date pattern, currently defined as "dddd, dd MMMM yyyy".
pD :: (HasDate (c cal), IsCalendar cal, Bounded (Month cal), Read (Month cal), Show (Month cal), Enum (Month cal), Show (DoW (c cal)), Enum (DoW (c cal)), Bounded (DoW (c cal))) => Pattern (c cal -> c cal) (c cal -> String) String
pD = pdddd <% string ", " <> pdd <% char ' ' <> pMMMM <% char ' ' <> pyyyy

-- | The ISO-8601 round-trippable date pattern, "yyyy-MM-dd".
pR :: HasDate d => Pattern (d -> d) (d -> String) String
pR = pyyyy <% char '-' <> pMM <% char '-' <> pdd

-- | Case-insensitive literal string parser, used by the name-based patterns ('pMMMM', 'pddd', 'pdddd').
caseInsensitiveString :: String -> Parsec String () String
caseInsensitiveString = mapM caseInsensitiveChar
  where
    caseInsensitiveChar c = (P.char (toLower c) <|> P.char (toUpper c)) >> return c