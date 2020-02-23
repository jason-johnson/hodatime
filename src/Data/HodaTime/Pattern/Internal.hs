{-# LANGUAGE DeriveFunctor #-}

module Data.HodaTime.Pattern.Internal
(
   PatternFor(..)
  ,Profunctor(..)
  ,Format(..)
  ,formatToString
  ,bchar
  ,hour'
  ,minute'
  ,second'
)
where

import Data.HodaTime.CalendarDateTime.Internal (LocalTime(..))
import Data.HodaTime.LocalTime.Internal (hour, minute, second)
import Data.Monoid ((<>))
import Control.Applicative (Const(..))
import            Data.Text.Lazy.Builder (Builder)
import qualified  Data.Text as T
import qualified  Data.Text.Lazy as TL
import qualified  Data.Text.Lazy.Builder as TLB

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
newtype Format r = Format { runFormat :: (Builder -> r) -> LocalTime -> r }

instance Monoid r => Monoid (Format r)
  where
    mempty = Format (\_ _ -> mempty)
    mappend m n = Format (\k a -> runFormat m (\b1 -> runFormat n (\b2 -> k (b1 <> b2)) a) a)

bchar :: Char -> Format r
bchar c = Format (. f)
  where
    f _ = TLB.fromText . T.singleton $ c

later :: (LocalTime -> Builder) -> Format r
later f = Format (. f)

lens :: ((Int -> Const Int Int) -> LocalTime -> Const Int LocalTime) -> Format r
lens l = later (TLB.fromText . T.pack . show . get l)
  where
    get :: ((s -> Const s c) -> a -> Const t b) -> a -> t
    get ll = getConst . ll Const

hour' :: Format r
hour' = lens hour

minute' :: Format r
minute' = lens minute

second' :: Format r
second' = lens second

formatToString :: Format [Char] -> LocalTime -> [Char]
formatToString m = runFormat m (TL.unpack . TLB.toLazyText)

-- Parsing

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