module HodaTime.InstantBench
(
  instantBenches
)
where

import Criterion.Main
import Data.List (foldl')
import qualified Data.HodaTime.Instant as I
import qualified Data.HodaTime.Duration as D

-- | Force an 'Instant' completely (its 'Show' touches every field), returning an 'Int' so criterion's 'nf' has
--   something fully-evaluable to hold on to.
forceI :: I.Instant -> Int
forceI = length . show

forceD :: D.Duration -> Int
forceD = length . show

instantBenches :: Benchmark
instantBenches = bgroup "Instant/Duration"
  [
     bench "add second"               $ nf (forceI . I.add base) oneSec
    ,bench "add nano"                 $ nf (forceI . I.add base) oneNano
    ,bench "minus second"            $ nf (forceI . I.minus base) oneSec
    ,bench "difference"               $ nf (forceD . I.difference far) base
    ,bench "duration add"             $ nf (forceD . D.add dSec) dNano
    ,bench "accumulate 10k adds sec"  $ nf (\n -> forceI (foldl' I.add base (replicate n oneSec))) 10000
    ,bench "accumulate 10k adds nano" $ nf (\n -> forceI (foldl' I.add base (replicate n oneNano))) 10000
  ]
  where
    base    = I.fromSecondsSinceUnixEpoch 1000000000
    far     = I.fromSecondsSinceUnixEpoch 1500000000
    oneSec  = D.fromSeconds 1
    oneNano = D.fromNanoseconds 1
    dSec    = D.fromSeconds 3600
    dNano   = D.fromNanoseconds 123456789
