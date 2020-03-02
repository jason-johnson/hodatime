{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Data.HodaTime.Pattern.Internal
(
   Pattern(..)
  ,DefaultForParse(..)
  ,parse
  ,parse'
  ,format
  ,(<>)         -- TODO: Remove
  ,(<%)
  ,(%>)
  ,pat_string
  ,pat_char
  ,pat_lens
  ,digitsToInt
  ,p_sixty
  ,f_shown
  ,f_shown_two
)
where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Typeable (Typeable)
import Data.Semigroup ((<>), Semigroup)
import qualified  Data.Text as T
import qualified  Data.Text.Lazy.Builder as TLB
import Text.Parsec hiding (many, optional, (<|>), parse)
import Formatting (Format, later, formatToString, left, (%.), (%), now)
import Data.String (fromString)
import Data.HodaTime.Internal.Lens (view, set, Lens)
import Data.HodaTime.Pattern.Defaults (DefaultForParse(..))

-- TODO: Remove these when we get %> fixed
import Formatting (runFormat)
import Formatting.Internal (Format(..))

type Parser a r = Parsec r () a

data ParseFailedException = ParseFailedException String
  deriving (Typeable, Show)

instance Exception ParseFailedException

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

-- | Merge a static pattern with one that operates on a data type (BUG: currently ignores the left pattern on format operations)
(%>) :: Pattern c r r -> Pattern (a -> a) (a -> r) r -> Pattern (a -> a) (a -> r) r
(Pattern parse1 format1) %> (Pattern parse2 format2) = Pattern par fmt
  where
    par = parse1 *> parse2
    fmt = Format $ runFormat format1 *> runFormat format2   -- TODO: discards the formatter on the left

instance Semigroup (Pattern (a -> a) (b -> r) r) where
  (Pattern parse1 format1) <> (Pattern parse2 format2) = Pattern par fmt
    where
      par = (.) <$> parse1 <*> parse2
      fmt = format1 `mappend` format2

parse :: (MonadThrow m, DefaultForParse a) => Pattern (a -> a) b String -> SourceName -> m a
parse pat s = parse' pat s getDefault

parse' :: MonadThrow m => Pattern (a -> a) b String -> SourceName -> a -> m a
parse' (Pattern p _) s def =
  case runParser p () s s of
    Left err -> throwM . ParseFailedException $ show err
    Right r -> return . r $ def

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