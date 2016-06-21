module Main (
    main
 ) where

import Criterion.Main
import Criterion.Types (Config(..))

import HodaTime.OffsetBench (offsetBenches)

main :: IO ()
main = defaultMainWith benchConfig [benches]

benches :: Benchmark
benches = bgroup "Benchmarks" [offsetBenches]

-- This configuration enables garbage collection between benchmarks. It is a
-- good idea to do so. Otherwise GC might distort your results
benchConfig :: Config
benchConfig = defaultConfig { forceGC = True }
