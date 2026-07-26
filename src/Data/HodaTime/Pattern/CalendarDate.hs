{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Data.HodaTime.Pattern.CalendarDate
(
  -- * Standard Patterns
   pd
  ,pD
  ,pR
  ,pmonthDay
  ,pyearMonth
  -- * Custom Patterns
  --
  -- | Used to create specialized patterns.
  ,pyear
  ,pyyyy
  ,pyy
  ,pmonthNum
  ,pMM
  ,pMMM
  ,pMMMM
  ,pday
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
import Text.Parsec (choice, try, Parsec, (<?>))
import qualified Text.Parsec as P (char)
import Formatting (later)
import Data.HodaTime.Internal.Lens (view, set)

-- d1 = maybe (error "duh") id $ calendarDate 1 January 2000
-- d2 = maybe (error "duh") id $ calendarDate 3 March 2020
-- format Data.HodaTime.Pattern.CalendarDate.date d1
-- format Data.HodaTime.Pattern.CalendarDate.date d2
-- parse Data.HodaTime.Pattern.CalendarDate.date "2000/March/01" :: IO (CalendarDate Gregorian)

-- | Absolute year of at least @w@ digits.  Width @1@ is the no-padding case (reads 1-4 digits, so both @"3"@ and
--   @"2020"@ parse; formats with no leading zeros); width @n >= 2@ reads exactly @n@ digits.  The value is always the
--   literal year and is never truncated, so this is the /strict/ counterpart to 'pyy' (which does two-digit century
--   inference).  Values 0-9999 (note: not all dates will be valid in all calendars, if the date is too early it will
--   clamp to earliest valid date)
pyear :: HasDate d => Int -> Pattern (d -> d) (d -> String) String
pyear w = pat_lens CDT.year (pDigits w 4 0 9999) (f_shown_pad w) "year: 0-9999"

-- | Absolute year in exactly 4 digits (@'pyear' 4@); values 0000-9999.
pyyyy :: HasDate d => Pattern (d -> d) (d -> String) String
pyyyy = pyear 4

-- | Two-digit year of the era with the century inferred, mirroring Noda Time's @yy@ specifier (contrast with the
--   strict, absolute 'pyear').  Formatting emits @year `mod` 100@ zero-padded to two digits, so @2020@ becomes @"20"@
--   and @2005@ becomes @"05"@.  Parsing reads exactly two digits and expands them to the year with those final two
--   digits that is closest to the parse /template/ (the default passed to 'parse', whose year is 2000 for the Gregorian
--   epoch), breaking ties toward the future.  With the default template this maps @"20"@ to @2020@ and @"99"@ to
--   @1999@; supply a different template via 'parse'' to slide the 100-year window.
pyy :: HasDate d => Pattern (d -> d) (d -> String) String
pyy = Pattern par fmt
  where
    par = expand <$> pDigits 2 2 0 99 <?> "year: two digits (century inferred)"
    expand v d = set CDT.year (fullYear (view CDT.year d) v) d
    fmt = f_shown_pad 2 (\d -> view CDT.year d `mod` 100)
    fullYear t v = base + k * 100
      where
        base = (t `div` 100) * 100 + v
        k = (t - base + 50) `div` 100

-- | Month of year as a number of @w@ digits, zero-padded; a width of @1@ means /no padding/.  Values 1-12.
pmonthNum :: HasDate d => Int -> Pattern (d -> d) (d -> String) String
pmonthNum w = pat_lens monthl (subtract 1 <$> pDigits w 2 1 12) fmt "month: 1-12"
  where
    fmt x = f_shown_pad w (succ . x)

-- | Month of year as a zero-padded number (@'pmonthNum' 2@); values 01-12.
pMM :: HasDate d => Pattern (d -> d) (d -> String) String
pMM = pmonthNum 2

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
-- | Day of month of @w@ digits, zero-padded; a width of @1@ means /no padding/.  Values 1-31.
pday :: HasDate d => Int -> Pattern (d -> d) (d -> String) String
pday w = pat_lens CDT.day (pDigits w 2 1 31) (f_shown_pad w) "day: 1-31"

-- | Day of month, zero-padded (@'pday' 2@); values 01-31.
pdd :: HasDate d => Pattern (d -> d) (d -> String) String
pdd = pday 2

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

-- | The month-and-day partial pattern (no year), currently "MMMM dd", e.g. @March 03@.
pmonthDay :: (HasDate (c cal), IsCalendar cal, Bounded (Month cal), Read (Month cal), Show (Month cal), Enum (Month cal)) => Pattern (c cal -> c cal) (c cal -> String) String
pmonthDay = pMMMM <% char ' ' <> pdd

-- | The year-and-month partial pattern (no day), currently "yyyy MMMM", e.g. @2020 March@.
pyearMonth :: (HasDate (c cal), IsCalendar cal, Bounded (Month cal), Read (Month cal), Show (Month cal), Enum (Month cal)) => Pattern (c cal -> c cal) (c cal -> String) String
pyearMonth = pyyyy <% char ' ' <> pMMMM

-- | Case-insensitive literal string parser, used by the name-based patterns ('pMMMM', 'pddd', 'pdddd').
caseInsensitiveString :: String -> Parsec String () String
caseInsensitiveString = mapM caseInsensitiveChar
  where
    caseInsensitiveChar c = (P.char (toLower c) <|> P.char (toUpper c)) >> return c