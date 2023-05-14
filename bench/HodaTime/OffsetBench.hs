module HodaTime.OffsetBench
(
  offsetBenches
)
where

import Criterion.Main
import Data.HodaTime.Offset (addClamped, fromHours)
import Data.HodaTime.Offset (addClamped, fromHours)

offsetBenches :: Benchmark
offsetBenches = bgroup "Offset Benchmarks" [addHour]

addHour :: Benchmark
addHour = bench "addClamped 1"  $ whnf (addClamped oneHour) twoHours
  where
    oneHour = fromHours (1 :: Int)
    twoHours = fromHours (2  :: Int)
