{-# LANGUAGE NoImplicitPrelude #-}

-- | Database models
--
-- @since 0.1.0
module Model
    ( Benchmark(Benchmark)
    , BenchmarkId
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
    , environmentBranch
    , environmentCommit
    , environmentPullRequest
    , environmentToken
    , migrateAll
    , testBenchmarks
    , testEnvironment
    , testTime
    ) where

import           ClassyPrelude.Yesod
                 ( Bool(True), Double, Int, Show, Text, UTCTime, drop, mkMigrate
                 , mkPersist, mpsGenerateLenses, persistFileWith, share
                 , sqlSettings )

import           Data.Aeson.TH
                 ( defaultOptions, deriveJSON, fieldLabelModifier )

import           Database.Persist.Quasi ( lowerCaseSettings )

share [ mkPersist sqlSettings { mpsGenerateLenses = True }
      , mkMigrate "migrateAll"
      ]
      $(persistFileWith lowerCaseSettings "config/models.persistentmodels")

-- | Drop the "_benchmark" from the Benchmark
deriveJSON defaultOptions { fieldLabelModifier = drop 10 } ''Benchmark

-- | Drop the "_result" from the Result
deriveJSON defaultOptions { fieldLabelModifier = drop 5 } ''Test

-- | Drop the "_result" from the Result
deriveJSON defaultOptions { fieldLabelModifier = drop 12 } ''Environment

-- | Get a new empty Benchmark instance
emptyBenchmark :: Benchmark
emptyBenchmark = Benchmark 0 0 "" 0 ""
