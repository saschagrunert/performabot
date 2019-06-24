-- | Database models
--
-- @since 0.1.0
module Model
    ( Benchmark(Benchmark)
    , BenchmarkId
    , Entry
    , Environment(Environment)
    , EnvironmentId
    , Test(Test)
    , TestId
    , benchmarkAverage
    , benchmarkDerivation
    , benchmarkName
    , benchmarkSamples
    , benchmarkUnit
    , emptyBenchmark
    , environmentCommit
    , environmentPullRequest
    , environmentRepoSlug
    , environmentToken
    , migrateAll
    , testBenchmarks
    , testEnvironment
    , testTime
    ) where

import           Data.Aeson.TH
                 ( defaultOptions, deriveJSON, fieldLabelModifier )
import           Data.Text           ( Text )
import           Data.Time           ( UTCTime )

import           Database.Persist.TH ( mkMigrate, mkPersist, mpsGenerateLenses
                                     , persistLowerCase, share, sqlSettings )

share [ mkPersist sqlSettings { mpsGenerateLenses = True }
      , mkMigrate "migrateAll"
      ]
      [persistLowerCase|
Test
    benchmarks  [BenchmarkId]
    environment EnvironmentId
    time        UTCTime
    deriving    Show

Environment
    commit      Text
    pullRequest Text
    repoSlug    Text
    token       Text
    deriving    Show

Benchmark
    average     Double
    derivation  Double
    name        Text
    samples     Int
    unit        Text
    deriving    Show
|]

type Entry = (Environment, [Benchmark])

-- | Drop the "_benchmark" from the Benchmark
deriveJSON defaultOptions { fieldLabelModifier = drop 10 } ''Benchmark

-- | Drop the "_result" from the Result
deriveJSON defaultOptions { fieldLabelModifier = drop 5 } ''Test

-- | Drop the "_environment" from the Environment
deriveJSON defaultOptions { fieldLabelModifier = drop 12 } ''Environment

-- | Get a new empty Benchmark instance
emptyBenchmark :: Benchmark
emptyBenchmark = Benchmark 0 0 "" 0 ""
