-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Pattern.ZonedDateTime
-- Copyright   :  (C) 2017 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- This is the module for patterns for 'ZonedDateTime'.  It contains all patterns needed to format and parse 'ZoneDateTime's.
--
-- === NOTE
--
-- This module currently doesn't work.  Our suggestion is to convert it first to a 'CalendarDateTime' with `toCalendarDateTime`
-- and use the patterns for that type.
----------------------------------------------------------------------------
module Data.HodaTime.Pattern.ZonedDateTime
(
  -- TODO: We don't expose these, they are building blocks
   pat_dayz
  ,pat_monthz
  ,pat_yearz
  ,ZonedDateTimeInfo(..)
  -- * Patterns
)
where

import Data.HodaTime.Pattern.Internal
import Data.HodaTime.CalendarDateTime.Internal (IsCalendar, Month)
import Data.HodaTime.ZonedDateTime.Internal (ZonedDateTime(..))
import qualified Data.HodaTime.ZonedDateTime.Internal as ZDT
import Control.Monad.Catch (MonadThrow, throwM)
import qualified  Data.Text as T
import qualified  Data.Text.Lazy.Builder as TLB
import Control.Applicative ((<|>))
import Text.Parsec (digit, count, string, choice, oneOf, (<?>))
import qualified Text.Parsec as P (char)
import Formatting (left, (%.), later)

data ZonedDateTimeInfo cal m =
  ZonedDateTimeInfo
  {
     day :: m Int
    ,month :: m (Month cal)
    ,year :: m Int
    ,hour :: Int
    ,minute :: Int
    ,second :: Int
    ,nanoSecond :: Int
    ,zone :: String
  }

-- d1 = maybe (error "duh") id $ on <$> localTime 1 2 3 0 <*> calendarDate 1 January 2000
-- d2 = maybe (error "duh") id $ on <$> localTime 1 2 3 0 <*> calendarDate 3 March 2020
-- z1 = utc >>= return . fromCalendarDateTimeLeniently d1
-- z2 = utc >>= return . fromCalendarDateTimeLeniently d2

pat_yearz :: (MonadThrow m, IsCalendar cal) => Int -> Pattern (ZonedDateTimeInfo cal m -> ZonedDateTimeInfo cal m) (ZonedDateTime cal -> String) String
pat_yearz c = Pattern p fmt
  where
    rep = replicate c
    zeros = rep '0'
    nines = rep '9'
    p = (\y -> \zdti -> zdti { year = pure y}) . read <$> count c digit <?> "year: " ++ zeros ++ "-" ++ nines
    fmt = left c '0' %. f_shown ZDT.year

pat_monthz :: IsCalendar cal => Int -> Pattern (ZonedDateTime cal -> ZonedDateTime cal) (ZonedDateTime cal -> String) String
pat_monthz = undefined

pat_dayz :: (MonadThrow m, IsCalendar cal) => Pattern (ZonedDateTimeInfo cal m -> ZonedDateTimeInfo cal m) (ZonedDateTime cal -> String) String
pat_dayz = Pattern p fmt
  where
    p = (\d -> \zdti -> zdti { day = pure d}) <$> (p_a <|> p_b) <?> "day: 01-31"
    p_a = digitsToInt <$> oneOf ['0'..'2'] <*> digit
    p_b = digitsToInt <$> P.char '3' <*> oneOf ['0', '1']
    fmt = left 2 '0' %. f_shown ZDT.day