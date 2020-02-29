{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.HodaTime.Pattern.Internal
(
   PatternFor(..)
  ,Profunctor(..)
  ,Format(..)
  ,formatToString
  ,now
  ,charr
  ,char
  ,hour'
  ,minute'
  ,second'
  ,p_hour
  ,p_hour_12
  ,p_minute
  ,p_second
  ,parse
  ,(<>)
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
import            Data.Text.Lazy.Builder (Builder)
import qualified  Data.Text as T
import qualified  Data.Text.Lazy as TL
import qualified  Data.Text.Lazy.Builder as TLB
import Text.Parsec hiding (many, optional, (<|>), parse)

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

-- Think about brining back the a type here because like this we're required to always
-- handle a LocalTime parameter and we don't always want that.  We can avoid giving out
-- any functions which are too loose anyway so no real reason to lock it down here
--
-- Well, having done it I see that it actually breaks the structure, at least so far.  This way you can't
-- mix constant builders like char with LocalTime ones like lens, hour', etc. because they have different
-- types
newtype Format r a = Format { runFormat :: (Builder -> r) -> a }

instance Monoid (Format r (a -> r)) where
  mappend m n = Format (\k a -> runFormat m (\b1 -> runFormat n (\b2 -> k (b1 <> b2)) a) a)
  mempty = Format (\k _ -> k mempty)

now :: Builder -> Format r r
now a = Format ($ a)

charr :: Char -> Format r r
charr c = now (TLB.fromText . T.singleton $ c)

later :: (a -> Builder) -> Format r (a -> r)
later f = Format (. f)

lens :: ((Int -> Const Int Int) -> LocalTime -> Const Int LocalTime) -> Format r (LocalTime -> r)
lens l = later (TLB.fromText . T.pack . show . get l)
  where
    get :: ((s -> Const s c) -> a -> Const t b) -> a -> t
    get ll = getConst . ll Const

hour' :: Format r (LocalTime -> r)
hour' = lens hour

minute' :: Format r (LocalTime -> r)
minute' = lens minute

second' :: Format r (LocalTime -> r)
second' = lens second

formatToString :: Format [Char] a -> a
formatToString m = runFormat m (TL.unpack . TLB.toLazyText)

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