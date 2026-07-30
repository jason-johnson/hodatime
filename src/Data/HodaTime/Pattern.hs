-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Pattern
-- Copyright   :  (C) 2016 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- A 'Pattern' is used to parse and format types in this library.
--
-- === Cookbook
--
-- ==== Format and parse with a standard pattern
--
-- A 'Pattern' both formats and parses.  @pR@ is the ISO-8601 round-trip date pattern.
--
-- >>> format pR <$> calendarDate 23 April 2024
-- Just "2024-04-23"
-- >>> parse pR "2024-04-23" :: Maybe (CalendarDate Gregorian)
-- Just (fromJust (Gregorian.calendarDate 23 April 2024))
--
-- ==== Build a custom pattern
--
-- Patterns compose: field patterns are joined with @\<>@ and literals inserted with @\<%@.  This builds a custom
-- \"23 Apr 2024\" layout (day, abbreviated month, year):
--
-- >>> format (pdd <% char ' ' <> pMMM <% char ' ' <> pyyyy) <$> calendarDate 23 April 2024
-- Just "23 Apr 2024"
----------------------------------------------------------------------------
module Data.HodaTime.Pattern
(
  -- * Types
   Pattern(..)
  -- * Parsing / Formatting
  ,parse
  ,parse'
  ,format
  -- * Standard Patterns
  -- * Custom Patterns
  --
  -- | Used to create specialized patterns
  ,string
  ,char
  ,(<%)
  -- * Exceptions
  ,ParseFailedException
)
where

import Data.HodaTime.Pattern.Internal

-- $setup
-- >>> import Data.HodaTime.Calendar.Gregorian (calendarDate, Month(..), Gregorian)
-- >>> import Data.HodaTime.CalendarDate (CalendarDate)
-- >>> import Data.HodaTime.Pattern.CalendarDate (pR, pdd, pMMM, pyyyy)