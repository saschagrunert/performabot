-- | The benchmark tests
--
-- @since 0.1.0
module BenchmarkSpec ( benchmarkSpec ) where

import           Benchmark
                 ( average, derivation, emptyBenchmark, name, samples, unit )

import           Control.Lens     ( (^.) )

import           Test.Tasty.Hspec
                 ( Spec, it, parallel, shouldBe, shouldContain )

-- Benchmark.hs related unit tests
benchmarkSpec :: Spec
benchmarkSpec = parallel $ do
    it "should succeed to create an empty Benchmark" $
        show emptyBenchmark `shouldContain` "Benchmark"

    it "should succeed to access a Benchmarks 'average" $
        emptyBenchmark ^. average `shouldBe` 0

    it "should succeed to access a Benchmarks 'derivation" $
        emptyBenchmark ^. derivation `shouldBe` 0

    it "should succeed to access a Benchmarks 'name'" $
        emptyBenchmark ^. name `shouldBe` ""

    it "should succeed to access a Benchmarks 'samples'" $
        emptyBenchmark ^. samples `shouldBe` 0

    it "should succeed to access a Benchmarks 'unit'" $
        emptyBenchmark ^. unit `shouldBe` ""
