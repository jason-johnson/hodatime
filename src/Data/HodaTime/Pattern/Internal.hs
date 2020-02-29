{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.HodaTime.Pattern.Internal
(
   PatternFor(..)
  ,Profunctor(..)
  ,char
  ,f_hour
  ,f_minute
  ,f_second
  ,p_hour
  ,p_hour_12
  ,p_minute
  ,p_second
  ,parse
  ,(<>)
  ,formatToString
  ,(%)
  ,colon
  ,pat_hour
  ,pat_minute
  ,pat_second
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

-- NOTE: The profunctor from Profunctors package brings in more than 10 dependancies to support things we're not going to be using
class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  dimap f g = lmap f . rmap g
  {-# INLINE dimap #-}

  lmap :: (a -> b) -> p b c -> p a c
  lmap f = dimap f id
  {-# INLINE lmap #-}
  
  rmap :: (b -> c) -> p a b -> p a c
  rmap = dimap id
  {-# INLINE rmap #-}

  {-# MINIMAL dimap | (lmap, rmap) #-}

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

colon :: Format r r
colon = now (fromString ":")

-- Parsing

type Parser a = Parsec String () a

data ParseFailedException = ParseFailedException String
  deriving (Typeable, Show)

instance Exception ParseFailedException

-- parse ((p_hour <* char ':') <> p_minute <> (char ':' *> p_second)) "10:20:30" 
--        >>= return . ($ d) >>= formatToString ((f_hour % colon) `mappend` f_minute `mappend` (colon % f_second))

instance Semigroup (Parser (a -> a)) where
  (<>) m n = (\f g -> \x -> g . f $ x) <$> m <*> n

parseOld :: MonadThrow m => Parser a -> SourceName -> m a
parseOld p s = case runParser p () s s of
        Left err -> throwM . ParseFailedException $ show err
        Right r -> return r

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

data Pattern a = Pattern
  {
     patParse :: Parser (a -> a)
    ,patFormat :: Format String (a -> String)
  }

instance Semigroup (Pattern a) where
  (Pattern parse1 format1) <> (Pattern parse2 format2) = Pattern par fmt
    where
      par = (\f g -> \x -> g . f $ x) <$> parse1 <*> parse2
      fmt = format1 `mappend` format2

parse :: MonadThrow m => Pattern a -> SourceName -> m (a -> a)
parse (Pattern p _) s =
  case runParser p () s s of
    Left err -> throwM . ParseFailedException $ show err
    Right r -> return r

--  (<>) m n = (\f g -> \x -> g . f $ x) <$> m <*> n

pat_hour :: Pattern LocalTime
pat_hour = Pattern p_hour f_hour

pat_minute :: Pattern LocalTime
pat_minute = Pattern p_minute f_minute

pat_second :: Pattern LocalTime
pat_second = Pattern p_second f_second

-- Codec stuff used for examples

data PatternFor r w b a = PatternJ
  {
     patternRead :: r a
    ,patternWrite :: b -> w a
  }
  deriving (Functor)

instance (Applicative r, Applicative w) => Applicative (PatternFor r w b) where
  pure x = PatternJ
            {
               patternRead = pure x
              ,patternWrite = \_ -> pure x
            }
  f <*> x = PatternJ
              {
                 patternRead = patternRead f <*> patternRead x
                ,patternWrite = \c -> patternWrite f c <*> patternWrite x c
              }

instance (Monad r, Monad w) => Monad (PatternFor r w b) where
  return = pure
  m >>= f = PatternJ
              {
                 patternRead = patternRead m >>= \x -> patternRead (f x)
                ,patternWrite = \c -> patternWrite m c >>= \x -> patternWrite (f x) c
              }

instance (Functor r, Functor w) => Profunctor (PatternFor r w) where
  dimap fRead fWrite (PatternJ readPat  writePat) =
    PatternJ
      {
         patternRead = fmap fWrite readPat
        ,patternWrite = fmap fWrite . writePat . fRead
      }