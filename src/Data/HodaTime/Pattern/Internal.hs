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

-- let t = "53:25:" in parse (p_second x' <* char ':' >>= \xx -> p_minute xx <* char ':') t t
-- parse ((p_second <* char ':') <> p_minute) "40:20" 

instance Semigroup (Parser (a -> a)) where
  (<>) m n = (\f g -> \x -> g . f $ x) <$> m <*> n

parse :: MonadThrow m => Parser a -> SourceName -> m a
parse p s = case runParser p () s s of
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

-- Parser is   MonadThrow m => a -> m LocalTime    but the problem is, how do we ensure that enough parts are in the pattern, or how do we default them if missing
-- Actually, we don't have to at compile time because we have the m type to return errors with the pattern

-- Codec stuff used for examples

data PatternFor r w b a = Pattern
  {
     patternRead :: r a
    ,patternWrite :: b -> w a
  }
  deriving (Functor)

instance (Applicative r, Applicative w) => Applicative (PatternFor r w b) where
  pure x = Pattern
            {
               patternRead = pure x
              ,patternWrite = \_ -> pure x
            }
  f <*> x = Pattern
              {
                 patternRead = patternRead f <*> patternRead x
                ,patternWrite = \c -> patternWrite f c <*> patternWrite x c
              }

instance (Monad r, Monad w) => Monad (PatternFor r w b) where
  return = pure
  m >>= f = Pattern
              {
                 patternRead = patternRead m >>= \x -> patternRead (f x)
                ,patternWrite = \c -> patternWrite m c >>= \x -> patternWrite (f x) c
              }

instance (Functor r, Functor w) => Profunctor (PatternFor r w) where
  dimap fRead fWrite (Pattern readPat  writePat) =
    Pattern
      {
         patternRead = fmap fWrite readPat
        ,patternWrite = fmap fWrite . writePat . fRead
      }