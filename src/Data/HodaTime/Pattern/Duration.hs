module Data.HodaTime.Pattern.Duration
(
  -- * Standard Patterns
   pDuration
  ,pDurationNano
)
where

import Data.HodaTime.Pattern.Internal (Pattern(..), p_sixty)
import Data.HodaTime.Duration.Internal (Duration(..))
import qualified Data.HodaTime.Duration as Dur (fromSeconds, fromNanoseconds, add)
import Data.HodaTime.Instant.Internal (Instant(..))
import Data.HodaTime.Constants (secondsPerDay, nsecsPerSecond)
import Formatting (Format, later)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TLB
import Text.Parsec (Parsec, many1, count, digit, option, (<?>))
import qualified Text.Parsec as P (char)

-- d1 = fromStandardDays 1 `Dur.add` fromSeconds 30
-- format pDuration d1        -- "1:00:00:30"
-- parse pDuration "-0:00:00:30"

type Parser a = Parsec String () a

-- DESIGN NOTE: like 'Data.HodaTime.Offset.Offset', a 'Duration' is a /signed/ quantity whose sign belongs to the
-- whole value, not to any one component — and it is stored floor-style (a signed day count plus /non-negative/
-- normalized sub-day seconds and nanoseconds, so e.g. -30s is stored as -1 day + 86370s).  So we do not decompose it
-- with independent field lenses; instead both directions treat it as a whole: the signed total is computed once (in
-- 'Integer', to interpret the floor representation correctly and to avoid overflow), then formatting emits the sign
-- followed by the magnitude, and parsing reads the sign and all components and rebuilds via 'Dur.fromSeconds' /
-- 'Dur.fromNanoseconds' (which re-normalize).  Hence the parser's setter is 'const': the text fully determines the
-- value.

-- | The duration pattern @[-]D:HH:mm:ss@ (days, hours, minutes, seconds; a leading @-@ only for negative durations),
--   e.g. @1:00:00:30@ or @-0:00:00:30@.
pDuration :: Pattern (Duration -> Duration) (Duration -> String) String
pDuration = Pattern (const <$> durationParser False <?> "duration: [-]D:HH:mm:ss") (durationFormat False)

-- | Like 'pDuration' but with a nanosecond fraction, @[-]D:HH:mm:ss.fffffffff@.
pDurationNano :: Pattern (Duration -> Duration) (Duration -> String) String
pDurationNano = Pattern (const <$> durationParser True <?> "duration: [-]D:HH:mm:ss.fffffffff") (durationFormat True)

-- helpers

-- | Decompose into @(isNegative, days, hours, minutes, seconds, nanoseconds)@ as non-negative magnitudes.
durComponents :: Duration -> (Bool, Integer, Integer, Integer, Integer, Integer)
durComponents (Duration (Instant days secs nsecs)) = (total < 0, d, h, m, s, ns)
  where
    nsPerSec = nsecsPerSecond :: Integer
    nsPerDay = (secondsPerDay :: Integer) * nsPerSec
    total = fromIntegral days * nsPerDay + fromIntegral secs * nsPerSec + fromIntegral nsecs
    a = abs total
    (totalSec, ns) = a `divMod` nsPerSec
    (totalMin, s)  = totalSec `divMod` 60
    (totalHr, m)   = totalMin `divMod` 60
    (d, h)         = totalHr `divMod` 24

durationFormat :: Bool -> Format String (Duration -> String)
durationFormat withFrac = later (TLB.fromText . T.pack . render)
  where
    render dur = sign ++ show d ++ ":" ++ pad2 h ++ ":" ++ pad2 m ++ ":" ++ pad2 s ++ fracPart
      where
        (neg, d, h, m, s, ns) = durComponents dur
        sign = if neg then "-" else ""
        fracPart = if withFrac then "." ++ padN 9 ns else ""
    pad2 = padN 2
    padN n x = let str = show x in replicate (n - length str) '0' ++ str

durationParser :: Bool -> Parser Duration
durationParser withFrac = do
  neg <- option False (True <$ P.char '-')
  d <- readInt <$> many1 digit
  _ <- P.char ':'
  h <- twoDigit
  _ <- P.char ':'
  m <- p_sixty
  _ <- P.char ':'
  s <- p_sixty
  ns <- if withFrac then P.char '.' *> (readInt <$> count 9 digit) else pure 0
  let sign = if neg then -1 else 1
      totalSec = sign * (((d * 24 + h) * 60 + m) * 60 + s)
      signedNs = sign * ns
  return $ Dur.fromSeconds totalSec `Dur.add` Dur.fromNanoseconds signedNs
  where
    twoDigit = readInt <$> count 2 digit
    readInt = read :: String -> Int
