-- | The go parser tests
--
-- @since 0.1.0
module ParserGoSpec ( parserGoSpec ) where

import           Control.Lens     ( (^.) )

import           Model
                 ( Benchmark, benchmarkAverage, benchmarkDerivation
                 , benchmarkName, benchmarkSamples, benchmarkUnit
                 , emptyBenchmark )

import           Parser           ( State(Failure, Init, NeedMore, Ok) )

import           ParserGo         ( parse )

import           Test.Tasty.Hspec
                 ( Spec, it, parallel, shouldBe, shouldContain )

-- Access the failure string of a given State
failure :: State -> String
failure (Failure s) = s
failure _ = ""

-- Access the benchmark
benchmark :: State -> Benchmark
benchmark (NeedMore b) = b
benchmark (Ok b) = b
benchmark _ = emptyBenchmark

-- ParserGo.hs related unit tests
parserGoSpec :: Spec
parserGoSpec = parallel $ do
    it "should succeed to parse" $ do
        let s0 = parse Init "  10 samples:"
        benchmark s0 ^. benchmarkSamples `shouldBe` 10
        let s1 = parse s0 $ "    pullTime - Fastest Time: 0.944s, "
                ++ "Average Time: 0.953s ± 0.008s, Slowest Time: 0.971s"
        let s2 = parse s1 $ "    other benchmark - Fastest Time: 0.944s, "
                ++ "Average Time: 0.123s ± 1.000s, Slowest Time: 0.971s"
        let r1 = benchmark s1
        let r2 = benchmark s2
        r1 ^. benchmarkAverage `shouldBe` 0.953
        r1 ^. benchmarkDerivation `shouldBe` 0.008
        r1 ^. benchmarkName `shouldBe` "pullTime"
        r1 ^. benchmarkSamples `shouldBe` 10
        r1 ^. benchmarkUnit `shouldBe` "s"
        r2 ^. benchmarkAverage `shouldBe` 0.123
        r2 ^. benchmarkDerivation `shouldBe` 1.000
        r2 ^. benchmarkName `shouldBe` "other benchmark"
        r2 ^. benchmarkSamples `shouldBe` 10
        r2 ^. benchmarkUnit `shouldBe` "s"

    it "should succeed to parse a huge sample number" $
        benchmark (parse Init " 192835128754 samples:")
        ^. benchmarkSamples `shouldBe` 192835128754

    it "should succeed to parse with ansi colors" $
        benchmark (parse Init " \ESC[1m20\ESC[0m samples:")
        ^. benchmarkSamples `shouldBe` 20

    it "should fail to parse empty input" $ failure (parse Init "")
        `shouldContain` "unexpected end of input"

    it "should fail to parse without space" $ failure (parse Init "10 samples:")
        `shouldContain` "expecting white space"

    it "should fail to parse without integer" $ failure (parse Init " wrong")
        `shouldContain` "expecting escape, integer, or white space"

    it "should fail to parse without 'samples'" $ failure (parse Init " 10 ")
        `shouldContain` "expecting \"samples:\""

    it "should fail to parse without colon" $ failure (parse Init " 10 samples")
        `shouldContain` "expecting \"samples:\""

    it "should fail to parse without eof" $
        failure (parse Init " 10 samples: wrong")
        `shouldContain` "expecting end of input"
