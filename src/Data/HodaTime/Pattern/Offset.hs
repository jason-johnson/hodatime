module Data.HodaTime.Pattern.Offset
(
  -- * Standard Patterns
   pOffset
  ,pOffsetFull
)
where

import Data.HodaTime.Pattern.Internal (Pattern(..), p_sixty)
import Data.HodaTime.Offset.Internal (Offset(..), fromSeconds)
import Data.HodaTime.Constants (secondsPerHour, secondsPerMinute)
import Formatting (Format, later)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TLB
import Text.Parsec (Parsec, count, digit, (<|>), (<?>))
import qualified Text.Parsec as P (char)

-- o1 = fromHours 2
-- format pOffset o1        -- "+02:00"
-- parse pOffset "-05:30"   -- Just (Offset -19800)

type Parser a = Parsec String () a

-- DESIGN NOTE (why this module doesn't build offsets from composable per-field patterns like the others do):
--
-- The date and time pattern modules build a value up field by field: each field is an independent lens 'set'
-- (year, month, day, hour, ...) and the '<>' combinator composes those setters.  That works because the fields
-- are independent, so any ordering produces the same result.
--
-- An 'Offset' does not decompose that cleanly.  It is a single *signed* count of seconds ('Offset Int'); there is
-- no separate "sign" field, and the sign belongs to the value as a whole rather than to any one component.  The
-- 'hours'/'minutes'/'seconds' component lenses split that total with Haskell's 'div'/'mod', which floor toward
-- negative infinity — so for a negative offset the parts are NOT the human "sign + magnitude" reading:
--
--     -5400 seconds  is  -(1h 30m),  i.e. it should display as "-01:30"
--     but   (-5400) `div` 3600 == -2      (the 'hours'   lens)
--       and (-5400) `mod` 3600 ==  1800   (=> the 'minutes' lens sees 30)
--
--   Reading the components independently would therefore render "-02:30" (wrong), and composing independent
--   per-field setters on a negative offset would build the wrong value entirely — it would also break the
--   order-independence the other patterns rely on, since the sign has to be applied to every component at once.
--
-- So both directions treat the offset as a whole:
--   * format works from 'abs secs' and emits the sign as a separate leading character;
--   * parse reads sign + all components together and multiplies the combined magnitude by the sign, building the
--     'Offset' through 'fromSeconds' (which also clamps to the +/-18h range).
--
-- Consequently the parsed value is fully determined by the text (it does not build on a default), which is why the
-- parser's setter is 'const <$> ...' — and these are exposed only as complete patterns, not composable sub-fields.

-- | The ISO-8601 offset pattern, sign followed by @HH:mm@ (e.g. @+02:00@, @-05:30@, @+00:00@ for UTC).
pOffset :: Pattern (Offset -> Offset) (Offset -> String) String
pOffset = Pattern (const <$> offsetParser False <?> "offset: (+/-)HH:mm") (offsetFormat False)

-- | Like 'pOffset' but including seconds, sign followed by @HH:mm:ss@ (e.g. @-05:30:15@).
pOffsetFull :: Pattern (Offset -> Offset) (Offset -> String) String
pOffsetFull = Pattern (const <$> offsetParser True <?> "offset: (+/-)HH:mm:ss") (offsetFormat True)

-- helpers

offsetParser :: Bool -> Parser Offset
offsetParser withSecs = do
  sign <- ((-1) <$ P.char '-') <|> (1 <$ P.char '+')
  h <- twoDigit
  _ <- P.char ':'
  m <- p_sixty
  s <- if withSecs then P.char ':' *> p_sixty else pure 0
  return . fromSeconds $ sign * (h * secondsPerHour + m * secondsPerMinute + s)
  where
    twoDigit = read <$> count 2 digit :: Parser Int

offsetFormat :: Bool -> Format String (Offset -> String)
offsetFormat withSecs = later (TLB.fromText . T.pack . render)
  where
    render (Offset secs) = sign : pad2 h ++ ":" ++ pad2 m ++ secPart
      where
        sign = if secs < 0 then '-' else '+'
        a = abs secs
        h = a `div` secondsPerHour
        m = (a `mod` secondsPerHour) `div` secondsPerMinute
        s = a `mod` secondsPerMinute
        secPart = if withSecs then ":" ++ pad2 s else ""
    pad2 x = let str = show x in if length str < 2 then '0' : str else str
