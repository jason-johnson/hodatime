-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Pattern
-- Copyright   :  (C) 2016 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  TBD
--
-- A 'Pattern' is used to parse and format types in this library.
--
-- === Cookbook
--
-- ==== Format and parse with a standard pattern
--
-- > import Data.HodaTime.Calendar.Gregorian (calendarDate, Month(..), Gregorian)
-- > import Data.HodaTime.CalendarDate (CalendarDate)
-- > import Data.HodaTime.Pattern (parse, format)
-- > import Data.HodaTime.Pattern.CalendarDate (pR)
-- >
-- > isoText   = format pR \<$\> calendarDate 23 April 2024                  -- Just \"2024-04-23\"
-- > roundTrip = parse pR \"2024-04-23\" :: Maybe (CalendarDate Gregorian)   -- Just \<23 April 2024\>
--
-- ==== Build a custom pattern
--
-- > import Data.HodaTime.Pattern (format, char, (<%))
-- > import Data.HodaTime.Pattern.CalendarDate (pdd, pMMM, pyyyy)
-- > import Data.Semigroup ((<>))
-- >
-- > -- a custom \"23 Apr 2024\" layout: day, abbreviated month, year
-- > shown = format (pdd <% char ' ' <> pMMM <% char ' ' <> pyyyy) \<$\> calendarDate 23 April 2024   -- Just \"23 Apr 2024\"
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