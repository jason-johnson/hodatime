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
  ,dimapP
  ,pairP
  ,string
  ,char
  ,pat_lens
  ,pat_lens'
  ,digitsToInt
  ,p_sixty
  ,f_shown
  ,f_shown_two
  ,f_shown_pad
  ,pDigits
  ,caseInsensitiveString
  ,ParseFailedException(..)
)
where

import Control.Monad.Catch (MonadThrow, throwM)
import qualified  Data.Text as T
import qualified  Data.Text.Lazy.Builder as TLB
import Text.Parsec hiding (many, optional, (<|>), parse, string, char)
import qualified Text.Parsec as P (string, char)
import Control.Applicative ((<|>))
import Data.Char (toLower, toUpper)
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

-- | Format a numeric field @n@ characters wide, zero-padded.  Width @1@ means no padding (every number is at least
--   one character, so @left 1 '0'@ never adds a zero).
f_shown_pad :: Show b => Int -> (a -> b) -> Format r (a -> r)
f_shown_pad n x = left n '0' %. f_shown x

-- | Parse a numeric field.  Width @1@ is the /no-padding/ case: it reads 1 up to @maxW@ digits, so both @"3"@ and
--   @"31"@ are accepted.  Width @n >= 2@ reads exactly @n@ digits.  Either way the value is validated to lie within
--   @[lo, hi]@.
pDigits :: Int -> Int -> Int -> Int -> Parser Int String
pDigits w maxW lo hi = do
  ds <- if w <= 1 then upTo maxW else count w digit
  let x = read ds
  if lo <= x && x <= hi then return x else parserFail ("expected " ++ show lo ++ "-" ++ show hi)
  where
    upTo :: Int -> Parser [Char] String
    upTo k = (:) <$> digit <*> go (k - 1)
    go :: Int -> Parser [Char] String
    go j = if j <= 0 then return [] else option [] ((:) <$> digit <*> go (j - 1))

-- | Case-insensitive literal string parser, used by the name-based patterns (month\/weekday names, AM\/PM designators).
caseInsensitiveString :: String -> Parsec String () String
caseInsensitiveString = mapM caseInsensitiveChar
  where
    caseInsensitiveChar :: Char -> Parsec String () Char
    caseInsensitiveChar c = (P.char (toLower c) <|> P.char (toUpper c)) >> return c

string :: String -> Pattern String String String
string s = Pattern p_str f_str
  where
    p_str = P.string s
    f_str = now (fromString s)

char :: Char -> Pattern Char String String
char c = Pattern p_char f_char
  where
    p_char = P.char c
    f_char = now (TLB.singleton c)

-- | Adapt a pattern over @s@ into a pattern over @t@ given a conversion in each direction (an isomorphism as far as the
--   pattern is concerned).  The parse setter is lifted through the conversion, and the formatter first projects the
--   @t@ down to an @s@.  This lets a pattern written for one type drive another that is convertible to it — for
--   example an 'Data.HodaTime.Instant.Instant' formatted through its UTC 'CalendarDateTime' projection.
dimapP :: (t -> s) -> (s -> t) -> Pattern (s -> s) (s -> String) String -> Pattern (t -> t) (t -> String) String
dimapP toS fromS (Pattern pS fmtS) = Pattern pT fmtT
  where
    pT = (\f -> fromS . f . toS) <$> pS
    fmtT = later (TLB.fromText . T.pack . formatToString fmtS . toS)

-- | Combine two patterns over independent parts (@a@ and @b@) of a whole @w@: the formatter concatenates their output
--   (the @a@ part then the @b@ part) and the parser runs both in that order and rebuilds the whole with the supplied
--   function.  Because @a@ and @b@ together fully determine @w@, the parsed setter is a 'const' (it does not build on
--   a default), so this produces whole values — e.g. an 'Data.HodaTime.OffsetDateTime.OffsetDateTime' from a
--   date\/time pattern and an offset pattern — rather than composable sub-fields.
pairP :: (DefaultForParse a, DefaultForParse b)
      => (w -> a) -> (w -> b) -> (a -> b -> w)
      -> Pattern (a -> a) (a -> String) String
      -> Pattern (b -> b) (b -> String) String
      -> Pattern (w -> w) (w -> String) String
pairP getA getB build (Pattern pa fa) (Pattern pb fb) = Pattern par fmt
  where
    par = (\sa sb -> const (build (sa getDefault) (sb getDefault))) <$> pa <*> pb
    fmt = later (\w -> TLB.fromText . T.pack $ formatToString fa (getA w) ++ formatToString fb (getB w))