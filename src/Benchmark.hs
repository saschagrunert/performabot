-- | Everything related to benchmark abstractions
--
-- @since 0.1.0
module Benchmark
    ( Benchmark(Benchmark)
    , average
    , derivation
    , emptyBenchmark
    , name
    , samples
    , unit
    ) where

import           Control.Lens ( makeLenses )

-- | A Benchmark result
data Benchmark =
    Benchmark { _average    :: Double  -- The average of the benchmark
              , _derivation :: Double  -- The standard derivation of the average
              , _name       :: String  -- The name of the benchmark
              , _samples    :: Integer -- The amount of sampled data
              , _unit       :: String  -- The unit of the benchmark, like "seconds"
              }
    deriving ( Show )

-- | Get a new empty Benchmark instance
emptyBenchmark :: Benchmark
emptyBenchmark = Benchmark 0 0 "" 0 ""

-- | Lens creation
makeLenses ''Benchmark
