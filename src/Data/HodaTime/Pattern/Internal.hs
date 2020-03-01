{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Data.HodaTime.Pattern.Internal
(
   Pattern(..)
  ,parse'
  ,format
  ,(<>)         -- TODO: Remove
  ,(<%)
 -- ,(%>)
  ,pat_string
  ,pat_char
  ,pat_lens
  ,digitsToInt
  ,p_sixty
  ,f_shown
  ,f_shown_two
)
where

import Data.HodaTime.CalendarDateTime.Internal (LocalTime)
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

-- x = maybe (error "duh") id $ localTime 1 2 3 0
-- d = maybe (error "duh") id $ localTime 0 0 0 0
-- pat = pat_hour <% pat_char ':' <> pat_minute <% pat_char ':' <> pat_second
-- parse' pat "01:01:01" d
-- format pat x

type Parser a r = Parsec r () a

data ParseFailedException = ParseFailedException String
  deriving (Typeable, Show)

instance Exception ParseFailedException

data Pattern a b r = Pattern
  {
     _patParse :: Parser a r
    ,_patFormat :: Format r b
  }

(<%) :: Pattern a b r -> Pattern c r r -> Pattern a b r
(Pattern parse1 format1) <% (Pattern parse2 format2) = Pattern par fmt
  where
    par = parse1 <* parse2
    fmt = format1 % format2

-- (%>) :: Pattern c r r -> Pattern (a -> a) (a -> r) r -> Pattern (a -> a) (a -> r) r
-- (Pattern parse1 format1) %> (Pattern parse2 format2) = Pattern par fmt
--  where
--    par = parse1 *> parse2
--    fmt = (format1 :: Format r r) % format2

-- BUG: The above fails with
-- Expected type: Format r1 r1
-- Actual type: Format r r

instance Semigroup (Pattern (a -> a) (b -> r) r) where
  (Pattern parse1 format1) <> (Pattern parse2 format2) = Pattern par fmt
    where
      par = (\f g -> \x -> g . f $ x) <$> parse1 <*> parse2
      fmt = format1 `mappend` format2

-- TODO: We want to have a function parse which gets a default a to use

parse' :: MonadThrow m => Pattern (a -> a) b String -> SourceName -> a -> m a
parse' (Pattern p _) s def =
  case runParser p () s s of
    Left err -> throwM . ParseFailedException $ show err
    Right r -> return . r $ def

format :: Pattern a r String -> r
format (Pattern _ fmt) = formatToString fmt

pat_lens :: Lens LocalTime LocalTime Int Int
              -> Parser Int String
              -> ((LocalTime -> Int) -> Format String (LocalTime -> String))
              -> String
              -> Pattern (LocalTime -> LocalTime) (LocalTime -> String) String
pat_lens l p f err = Pattern par fmt
  where
    fmt = f $ view l
    par = set l <$> p <?> err

digitsToInt :: Char -> Char -> Int
digitsToInt a b = read [a, b]

p_sixty :: Parser Int String
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