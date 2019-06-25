-- | Database models
--
-- @since 0.1.0
module Model
    ( Benchmark(Benchmark)
    , Benchmarks
    , BenchmarkId
    , Test(Test)
    , TestId
    , benchmarkAverage
    , benchmarkDerivation
    , benchmarkName
    , benchmarkSamples
    , benchmarkUnit
    , emptyBenchmark
    , migrateAll
    , testBenchmarks
    , testCommit
    , testPullRequest
    , testRepoSlug
    , testTime
    ) where

import           Data.Aeson.TH
                 ( defaultOptions, deriveJSON, fieldLabelModifier )
import           Data.Text              ( Text )
import           Data.Time              ( UTCTime )

import           Database.Persist.Quasi ( lowerCaseSettings )
import           Database.Persist.TH
                 ( mkMigrate, mkPersist, mpsGenerateLenses, persistFileWith
                 , share, sqlSettings )

share [ mkPersist sqlSettings { mpsGenerateLenses = True }
      , mkMigrate "migrateAll"
      ]
      $(persistFileWith lowerCaseSettings "src/model")

-- | Multiple Benchmarks
type Benchmarks = [Benchmark]

-- | Drop the "_benchmark" from the Benchmark
deriveJSON defaultOptions { fieldLabelModifier = drop 10 } ''Benchmark

-- | Drop the "_result" from the Result
deriveJSON defaultOptions { fieldLabelModifier = drop 5 } ''Test

-- | Get a new empty Benchmark instance
emptyBenchmark :: Benchmark
emptyBenchmark = Benchmark 0 0 "" 0 ""
