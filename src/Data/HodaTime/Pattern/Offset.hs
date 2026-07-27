module Data.HodaTime.Pattern.Offset
(
  -- * Standard Patterns
   pOffset
  ,pOffsetZ
  ,pOffsetFull
  ,pOffsetCompact
)
where

import Data.HodaTime.Pattern.Internal (Pattern(..), p_sixty)
import Data.HodaTime.Offset.Internal (Offset(..), fromSeconds)
import Data.HodaTime.Constants (secondsPerHour, secondsPerMinute)
import Formatting (Format, later)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TLB
import Text.Parsec (Parsec, count, digit, (<|>), (<?>))
import qualified Text.Parsec as P (char, string)

-- o1 = fromHours 2
-- format pOffset o1        -- "+02:00"
-- parse pOffset "-05:30"   -- Just (Offset -19800)

type Parser a = Parsec String () a

-- DESIGN NOTE (why this module doesn't build offsets from composable per-field patterns like the others do):
--
-- The date and time pattern modules build a value field by field: each field is an independent lens 'set' and the
-- '<>' combinator composes those setters.  That works because those fields are independent and are backed by
-- read/write lenses.
--
-- An 'Offset' offers neither.  Its 'hours'/'minutes'/'seconds' are read-only accessors, not lenses — there is no
-- coherent 'set' for a single component of a signed quantity (see the note in Data.HodaTime.Offset) — so there is
-- nothing for the field-composition machinery to drive.  And the value is a single *signed* second count whose sign
-- belongs to the whole, not to any one component, so the components could not be set independently in any case.
--
-- So both directions treat the offset as a whole: format works from 'abs secs' and emits the sign as a separate
-- leading character; parse reads the sign and all components together and multiplies the combined magnitude by the
-- sign, building the 'Offset' through 'fromSeconds' (which also clamps to the +/-18h range).  The parsed value is
-- therefore fully determined by the text (it does not build on a default), which is why the parser's setter is
-- 'const <$> ...' — and these are exposed only as complete patterns, not composable sub-fields.

-- | The ISO-8601 offset pattern, sign followed by @HH:mm@ (e.g. @+02:00@, @-05:30@, @+00:00@ for UTC).
pOffset :: Pattern (Offset -> Offset) (Offset -> String) String
pOffset = Pattern (const <$> offsetParser ":" False <?> "offset: (+/-)HH:mm") (offsetFormat ":" False)

-- | Like 'pOffset' but including seconds, sign followed by @HH:mm:ss@ (e.g. @-05:30:15@).
pOffsetFull :: Pattern (Offset -> Offset) (Offset -> String) String
pOffsetFull = Pattern (const <$> offsetParser ":" True <?> "offset: (+/-)HH:mm:ss") (offsetFormat ":" True)

-- | Like 'pOffset' but renders UTC (a zero offset) as @Z@ rather than @+00:00@, per ISO-8601 (and parses @Z@ back).
pOffsetZ :: Pattern (Offset -> Offset) (Offset -> String) String
pOffsetZ = Pattern (const <$> parZ <?> "offset: Z or (+/-)HH:mm") fmtZ
  where
    parZ = (fromSeconds (0 :: Int) <$ P.char 'Z') <|> offsetParser ":" False
    fmtZ = later (\o -> TLB.fromText . T.pack $ if offsetSeconds o == 0 then "Z" else renderOffset ":" False o)

-- | The @strftime@ @%z@ offset: sign followed by @HHmm@ with no separator (e.g. @+0200@, @-0530@, @+0000@ for UTC).
pOffsetCompact :: Pattern (Offset -> Offset) (Offset -> String) String
pOffsetCompact = Pattern (const <$> offsetParser "" False <?> "offset: (+/-)HHmm") (offsetFormat "" False)

-- helpers

offsetParser :: String -> Bool -> Parser Offset
offsetParser sep withSecs = do
  sign <- ((-1) <$ P.char '-') <|> (1 <$ P.char '+')
  h <- twoDigit
  _ <- P.string sep
  m <- p_sixty
  s <- if withSecs then P.string sep *> p_sixty else pure 0
  return . fromSeconds $ sign * (h * secondsPerHour + m * secondsPerMinute + s)
  where
    twoDigit = read <$> count 2 digit :: Parser Int

offsetFormat :: String -> Bool -> Format String (Offset -> String)
offsetFormat sep withSecs = later (TLB.fromText . T.pack . renderOffset sep withSecs)

renderOffset :: String -> Bool -> Offset -> String
renderOffset sep withSecs (Offset secs) = sign : pad2 h ++ sep ++ pad2 m ++ secPart
  where
    sign = if secs < 0 then '-' else '+'
    a = abs secs
    h = a `div` secondsPerHour
    m = (a `mod` secondsPerHour) `div` secondsPerMinute
    s = a `mod` secondsPerMinute
    secPart = if withSecs then sep ++ pad2 s else ""
    pad2 x = let str = show x in if length str < 2 then '0' : str else str
