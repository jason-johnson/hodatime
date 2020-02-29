{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.HodaTime.Pattern.Internal
(
   parse'
  ,format
  ,(<>)         -- TODO: Remove
  ,(<%)
 -- ,(%>)
  ,pat_hour
  ,pat_hour_12
  ,pat_minute
  ,pat_second
  ,pat_char
)
where

import Data.HodaTime.CalendarDateTime.Internal (LocalTime(..))
import Data.HodaTime.LocalTime.Internal (hour, minute, second)
import Data.Functor.Identity (Identity(..))
import Control.Applicative (Const(..), (<|>))
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Typeable (Typeable)
import Data.Semigroup ((<>), Semigroup)
import qualified  Data.Text as T
import qualified  Data.Text.Lazy.Builder as TLB
import Text.Parsec hiding (many, optional, (<|>), parse)
import Formatting (Format, later, formatToString, left, (%.), (%), now)
import Data.String (fromString)

-- x = maybe (error "duh") id $ localTime 1 2 3 0
-- d = maybe (error "duh") id $ localTime 0 0 0 0
-- pat = pat_hour <% pat_char ':' <> pat_minute <% pat_char ':' <> pat_second
-- parse' pat "01:01:01" d
-- format pat x

-- Formatting

f_lens :: ((Int -> Const Int Int) -> LocalTime -> Const Int LocalTime) -> Format r (LocalTime -> r)
f_lens l = later (TLB.fromText . T.pack . show . get l)
  where
    get :: ((s -> Const s c) -> a -> Const t b) -> a -> t
    get ll = getConst . ll Const

f_hour :: Format r (LocalTime -> r)
f_hour = left 2 '0' %. f_lens hour

f_minute :: Format r (LocalTime -> r)
f_minute = left 2 '0' %. f_lens minute

f_second :: Format r (LocalTime -> r)
f_second = left 2 '0' %. f_lens second

-- Parsing

type Parser a = Parsec String () a

data ParseFailedException = ParseFailedException String
  deriving (Typeable, Show)

instance Exception ParseFailedException

p_sixty :: Parser Int
p_sixty = f <$> oneOf ['0'..'5'] <*> digit
  where
    f a b = read [a,b]

p_lens :: Parser Int -> ((Int -> Identity Int) -> LocalTime -> Identity LocalTime) -> String -> Parser (LocalTime -> LocalTime)
p_lens p ll err = f <$> p <?> err
  where
    f s = set s ll
    modify g l = runIdentity . l (Identity . g)
    set v = modify (const v)

p_second :: Parser (LocalTime -> LocalTime)
p_second = p_lens p_sixty second "second: 00-59"

p_minute :: Parser (LocalTime -> LocalTime)
p_minute = p_lens p_sixty minute "minute: 00-59"

p_hour :: Parser (LocalTime -> LocalTime)
p_hour = p_lens (p_a <|> p_b) hour "hour: 00-24"
  where
    p_a = f <$> oneOf ['0', '1'] <*> digit 
    p_b = f <$> char '2' <*> oneOf ['0'..'3']
    f a b = read [a,b]

p_hour_12 :: Parser (LocalTime -> LocalTime)
p_hour_12 = p_lens (p_a <|> p_b) hour "hour: 00-12"
  where
    p_a = f <$> char '0' <*> digit
    p_b = f <$> char '1' <*> oneOf ['0'..'2']
    f a b = read [a,b]

-- Pattern

type Parser2 a r = Parsec r () a

data Pattern a b r = Pattern
  {
     _patParse :: Parser2 a r
    ,_patFormat :: Format r b
  }

(<%) :: Pattern a b r -> Pattern c r r -> Pattern a b r
(Pattern parse1 format1) <% (Pattern parse2 format2) = Pattern par fmt
  where
    par = parse1 <* parse2
    fmt = format1 % format2

-- (%>) :: Pattern c r r -> Pattern a b r -> Pattern a b r
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

pat_hour :: Pattern (LocalTime -> LocalTime) (LocalTime -> String) String
pat_hour = Pattern p_hour f_hour

pat_hour_12 :: Pattern (LocalTime -> LocalTime) (LocalTime -> String) String
pat_hour_12 = Pattern p_hour_12 f_hour

pat_minute :: Pattern (LocalTime -> LocalTime) (LocalTime -> String) String
pat_minute = Pattern p_minute f_minute

pat_second :: Pattern (LocalTime -> LocalTime) (LocalTime -> String) String
pat_second = Pattern p_second f_second

pat_char :: Char -> Pattern Char String String
pat_char c = Pattern p_char f_char
  where
    p_char = char c
    f_char = now (fromString $ c:[])