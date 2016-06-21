module HodaTime.OffsetBench
(
  offsetBenches
)
where

import Criterion.Main
import Data.HodaTime.Offset (add, fromHours)

offsetBenches :: Benchmark
offsetBenches = bgroup "Offset Benchmarks" [addHour]

addHour :: Benchmark
addHour = bench "add 1"  $ whnf (add oneHour) twoHours
  where
    oneHour = fromHours (1 :: Int)
    twoHours = fromHours (2  :: Int)
