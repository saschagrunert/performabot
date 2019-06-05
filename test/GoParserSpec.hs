-- | The go parser tests
--
-- @since 0.1.0
module GoParserSpec ( goParserSpec ) where

import           Benchmark        ( Benchmark, average, derivation
                                  , emptyBenchmark, name, samples, unit )

import           Control.Lens     ( (^.) )

import           GoParser         ( parse )

import           Parser           ( State(Failure, Init, NeedMore, Ok) )

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

-- GoParser.hs related unit tests
goParserSpec :: Spec
goParserSpec = parallel $ do
    it "should succeed to parse" $ do
        let s = parse Init "  10 samples:"
        benchmark s ^. samples `shouldBe` 10
        let res = benchmark . parse s $ "    pullTime - Fastest Time: 0.944s, "
                ++ "Average Time: 0.953s ± 0.008s, Slowest Time: 0.971s"
        res ^. average `shouldBe` 0.953
        res ^. derivation `shouldBe` 0.008
        res ^. name `shouldBe` "pullTime"
        res ^. samples `shouldBe` 10
        res ^. unit `shouldBe` "s"

    it "should succeed to parse a huge sample number" $
        benchmark (parse Init " 192835128754 samples:")
        ^. samples `shouldBe` 192835128754

    it "should fail to parse empty input" $ failure (parse Init "")
        `shouldContain` "unexpected end of input"

    it "should fail to parse without space" $ failure (parse Init "10 samples:")
        `shouldContain` "expecting white space"

    it "should fail to parse without integer" $ failure (parse Init " wrong")
        `shouldContain` "expecting integer"

    it "should fail to parse without 'samples'" $ failure (parse Init " 10 ")
        `shouldContain` "expecting \"samples\""

    it "should fail to parse without colon" $ failure (parse Init " 10 samples")
        `shouldContain` "expecting ':'"

    it "should fail to parse without eof" $
        failure (parse Init " 10 samples: wrong")
        `shouldContain` "expecting end of input"
