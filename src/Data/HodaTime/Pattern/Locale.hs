{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :  Data.HodaTime.Pattern.Locale
-- Copyright   :  (C) 2016 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX (Linux, macOS)
--
-- Compiles the operating system's @strftime@-style layout strings (the @D_FMT@ \/ @T_FMT@ captured in a 'Locale' by
-- "Data.HodaTime.Locale") into hodatime 'Pattern's, so a date or time can be formatted and parsed using the machine's
-- own conventions.
--
-- ==== __Example__
--
-- > do loc <- currentLocale
-- >    p   <- localeDatePattern loc          -- e.g. "%d/%m/%Y" in fr_FR
-- >    pure (format p someDate)              -- "27/07/2026"
--
-- Only the single-category formats are handled: 'localeDatePattern' (from 'rawDateFormat') and 'localeTimePattern'
-- (from 'rawTimeFormat').  The combined @D_T_FMT@ is not yet supported because it mixes date and time fields and carries
-- a time-zone specifier (@%Z@) that a plain date\/time value has no slot for.
module Data.HodaTime.Pattern.Locale
(
   StrftimeError(..)
  ,localeDatePattern
  ,localeTimePattern
  ,compileDatePattern
  ,compileTimePattern
)
where

import Data.HodaTime.Pattern.Internal (Pattern(..))
import Data.HodaTime.Pattern.CalendarDate (pyyyy, pyy, pMM, pdd, pMMMM', pMMM', pdddd', pddd')
import Data.HodaTime.Pattern.LocalTime (pHH, phh, pmm, pss, ppp')
import Data.HodaTime.Locale.Internal (Locale(..))
import Data.HodaTime.CalendarDateTime.Internal (HasDate, DoW)
import Data.HodaTime.LocalTime.Internal (HasLocalTime)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TLB
import qualified Text.Parsec as P (string)
import Formatting (later)

-- | Raised when a layout string uses a @strftime@ conversion that the compiler does not implement.
data StrftimeError
  = UnsupportedSpecifier Char   -- ^ a conversion we do not support here (e.g. @%Z@, @%V@, or a width\/flag like @%-d@)
  | DanglingPercent             -- ^ the layout string ended with a bare @%@
  deriving (Eq, Show, Typeable)

instance Exception StrftimeError

-- | A literal chunk of the layout as a field pattern: it consumes\/emits the given text and leaves the value untouched.
--   Because it has the ordinary field-pattern shape it composes with '<>' like any other field, so literals anywhere in
--   the layout (including at the very start) need no special handling.
litField :: String -> Pattern (a -> a) (a -> String) String
litField s = Pattern (id <$ P.string s) (later (const (TLB.fromText (T.pack s))))

-- | A single token of a layout string, after composite specifiers have been expanded.
data Tok = Lit Char | Conv Char

-- | A run of literal characters or a single conversion specifier.
data Frag = LitRun String | ConvF Char

-- | Tokenise a layout string, expanding the composite specifiers (@%T@, @%R@, @%r@, @%F@, @%D@) into their parts.
tokenize :: String -> Either StrftimeError [Tok]
tokenize [] = Right []
tokenize ('%':c:rest) = case c of
  '%' -> (Lit '%' :)  <$> tokenize rest
  'n' -> (Lit '\n' :) <$> tokenize rest
  't' -> (Lit '\t' :) <$> tokenize rest
  'T' -> tokenize ("%H:%M:%S" ++ rest)
  'R' -> tokenize ("%H:%M" ++ rest)
  'r' -> tokenize ("%I:%M:%S %p" ++ rest)
  'F' -> tokenize ("%Y-%m-%d" ++ rest)
  'D' -> tokenize ("%m/%d/%y" ++ rest)
  _   -> (Conv c :) <$> tokenize rest
tokenize ['%'] = Left DanglingPercent
tokenize (c:rest) = (Lit c :) <$> tokenize rest

-- | Merge adjacent literal characters into runs so each becomes a single 'litField'.
toFrags :: [Tok] -> [Frag]
toFrags = foldr step []
  where
    step (Lit c)  (LitRun s : fs) = LitRun (c : s) : fs
    step (Lit c)  fs              = LitRun [c] : fs
    step (Conv c) fs              = ConvF c : fs

-- | Compile a layout string into a pattern, given a mapping from conversion specifiers to field patterns.
compileWith
  :: (Char -> Either StrftimeError (Pattern (a -> a) (a -> String) String))
  -> String
  -> Either StrftimeError (Pattern (a -> a) (a -> String) String)
compileWith mapConv fmtStr = do
  toks  <- tokenize fmtStr
  frags <- mapM toPat (toFrags toks)
  case frags of
    [] -> Right (litField "")
    _  -> Right (foldr1 (<>) frags)
  where
    toPat (LitRun s) = Right (litField s)
    toPat (ConvF c)  = mapConv c

dateConv :: (HasDate d, Enum (DoW d)) => Locale -> Char -> Either StrftimeError (Pattern (d -> d) (d -> String) String)
dateConv loc c = case c of
  'Y' -> Right pyyyy
  'y' -> Right pyy
  'm' -> Right pMM
  'd' -> Right pdd
  'B' -> Right (pMMMM' loc)
  'b' -> Right (pMMM' loc)
  'h' -> Right (pMMM' loc)
  'A' -> Right (pdddd' loc)
  'a' -> Right (pddd' loc)
  _   -> Left (UnsupportedSpecifier c)

timeConv :: HasLocalTime lt => Locale -> Char -> Either StrftimeError (Pattern (lt -> lt) (lt -> String) String)
timeConv loc c = case c of
  'H' -> Right pHH
  'I' -> Right phh
  'M' -> Right pmm
  'S' -> Right pss
  'p' -> Right (ppp' loc)
  _   -> Left (UnsupportedSpecifier c)

-- | Compile an explicit @strftime@ date layout against a 'Locale' (the locale supplies month\/weekday names for @%B@,
--   @%b@, @%A@, @%a@).  Throws a 'StrftimeError' on an unsupported specifier.
compileDatePattern :: (MonadThrow m, HasDate d, Enum (DoW d)) => Locale -> String -> m (Pattern (d -> d) (d -> String) String)
compileDatePattern loc = either throwM return . compileWith (dateConv loc)

-- | Compile an explicit @strftime@ time layout against a 'Locale' (the locale supplies the AM\/PM designators for
--   @%p@).  Throws a 'StrftimeError' on an unsupported specifier.
compileTimePattern :: (MonadThrow m, HasLocalTime lt) => Locale -> String -> m (Pattern (lt -> lt) (lt -> String) String)
compileTimePattern loc = either throwM return . compileWith (timeConv loc)

-- | The locale's short date pattern, compiled from its @D_FMT@ (POSIX @rawDateFormat@).
localeDatePattern :: (MonadThrow m, HasDate d, Enum (DoW d)) => Locale -> m (Pattern (d -> d) (d -> String) String)
localeDatePattern loc = compileDatePattern loc (rawDateFormat loc)

-- | The locale's time pattern, compiled from its @T_FMT@ (POSIX @rawTimeFormat@).
localeTimePattern :: (MonadThrow m, HasLocalTime lt) => Locale -> m (Pattern (lt -> lt) (lt -> String) String)
localeTimePattern loc = compileTimePattern loc (rawTimeFormat loc)
