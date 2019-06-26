{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Database models
--
-- @since 0.1.0
module Model where

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

-- | Get a new empty Benchmark instance
emptyBenchmark :: Benchmark
emptyBenchmark = Benchmark 0 0 "" 0 ""
