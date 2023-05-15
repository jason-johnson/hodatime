{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Data.HodaTime.Pattern.Internal
(
   Pattern(..)
  ,DefaultForParse(..)
  ,parse
  ,parse'
  ,parse''
  ,format
  ,(<>)         -- TODO: Remove
  ,(<%)
  ,pat_string
  ,pat_char
  ,pat_lens
  ,pat_lens'
  ,digitsToInt
  ,p_sixty
  ,f_shown
  ,f_shown_two
  ,ParseFailedException(..)
)
where

import Control.Monad.Catch (MonadThrow, throwM)
import qualified  Data.Text as T
import qualified  Data.Text.Lazy.Builder as TLB
import Text.Parsec hiding (many, optional, (<|>), parse)
import Formatting (Format, later, formatToString, left, (%.), (%), now)
import Data.String (fromString)
import Data.HodaTime.Internal.Lens (view, set, Lens)
import Data.HodaTime.Pattern.ApplyParse (DefaultForParse(..), ApplyParse(..))
import Control.Exception (Exception)
import Data.Typeable (Typeable)

-- Exceptions

-- | Parse failed on the given string
newtype ParseFailedException = ParseFailedException String
  deriving (Typeable, Show)

instance Exception ParseFailedException

type Parser a r = Parsec r () a

-- | Pattern for the data type which is used by the 'parse', 'format' and 'parse\'' functions
data Pattern a b r = Pattern
  {
     _patParse :: Parser a r
    ,_patFormat :: Format r b
  }

-- | Merge a pattern that operates on a data type with a static pattern
(<%) :: Pattern a b r -> Pattern c r r -> Pattern a b r
(Pattern parse1 format1) <% (Pattern parse2 format2) = Pattern par fmt
  where
    par = parse1 <* parse2
    fmt = format1 % format2

{-
-- | Merge a static pattern with one that operates on a data type

-- NOTE: The following doesn't work, I believe because of how much we're fixing the types removes the ability to apply (%) in either direction.
-- NOTE: But in fact, (<%) above is sufficient, the library can work fine without offering the other option

(%>) :: Pattern c r r -> Pattern a b r -> Pattern a b r
(Pattern parse1 format1) %> (Pattern parse2 format2) = Pattern par fmt
  where
    par = parse1 *> parse2
    fmt = format1 % format2
-}

instance Semigroup (Pattern (a -> a) (b -> r) r) where
  (Pattern parse1 format1) <> (Pattern parse2 format2) = Pattern par fmt
    where
      par = (.) <$> parse1 <*> parse2
      fmt = format1 `mappend` format2

-- | Parse a 'String' given by 'Pattern' for the data type 'a'.  Will call 'throwM' on failure.
-- NOTE: A default 'a' will be used to determine what happens for fields which do not appear in
--       the parse
parse :: (MonadThrow m, DefaultForParse a) => Pattern (a -> a) b String -> SourceName -> m a
parse pat s = parse' pat s getDefault

-- | Like 'parse' above but lets the user provide an 'a' as the default to use
parse' :: MonadThrow m => Pattern (a -> a) b String -> SourceName -> a -> m a
parse' (Pattern p _) s def =
  case runParser p () s s of
    Left err -> throwM . ParseFailedException $ show err
    Right r -> return . r $ def

parse'' :: (MonadThrow m, ApplyParse a b) => Pattern (a -> a) (b -> String) String -> SourceName -> m b
parse'' (Pattern p _) s =
  case runParser p () s s of
    Left err -> throwM . ParseFailedException $ show err
    Right r -> applyParse r

-- | Use the given 'Pattern' to format the data type 'a' into a 'String'
format :: Pattern a r String -> r
format (Pattern _ fmt) = formatToString fmt

pat_lens :: Lens s s a a
              -> Parser a String
              -> ((s -> a) -> Format String (s -> String))
              -> String
              -> Pattern (s -> s) (s -> String) String
pat_lens l p f err = Pattern par fmt
  where
    fmt = f $ view l
    par = set l <$> p <?> err

pat_lens' :: Lens s s a a
              -> Lens s' s' a' a'
              -> Parser a String
              -> ((s' -> a') -> Format String (s' -> String))
              -> String
              -> Pattern (s -> s) (s' -> String) String
pat_lens' lp lf p f err = Pattern par fmt
  where
    fmt = f $ view lf
    par = set lp <$> p <?> err

digitsToInt :: (Num n, Read n) => Char -> Char -> n
digitsToInt a b = read [a, b]

p_sixty :: (Num n, Read n) => Parser n String
p_sixty = digitsToInt <$> oneOf ['0'..'5'] <*> digit

f_shown :: Show b => (a -> b) -> Format r (a -> r)
f_shown x = later (TLB.fromText . T.pack . show . x)

f_shown_two :: Show b => (a -> b) -> Format r (a -> r)
f_shown_two x = left 2 '0' %. f_shown x

pat_string :: String -> Pattern String String String
pat_string s = Pattern p_str f_str
  where
    p_str = string s
    f_str = now (fromString s)

pat_char :: Char -> Pattern Char String String
pat_char c = Pattern p_char f_char
  where
    p_char = char c
    f_char = now (TLB.singleton c)