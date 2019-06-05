-- | The main test module
--
-- @since 0.1.0
module Main ( main ) where

import           BenchmarkSpec         ( benchmarkSpec )

import           GoParserSpec          ( goParserSpec )

import           ParserSpec            ( parserProps, parserSpec )

import           Test.Tasty
                 ( TestTree, defaultMain, localOption, testGroup )
import           Test.Tasty.Hspec      ( testSpec )
import           Test.Tasty.QuickCheck ( QuickCheckTests(QuickCheckTests) )

-- The main test routine
main :: IO ()
main = do
    u <- unitTests
    defaultMain . opts $ testGroup "Tests" [ u, propertyTests ]
  where
    opts = localOption $ QuickCheckTests 5000

-- Unit tests based on hspec
unitTests :: IO TestTree
unitTests = do
    benchmarkUnitTests <- testSpec "Benchmark.hs" benchmarkSpec
    goParserUnitTests <- testSpec "GoParser.hs" goParserSpec
    parserUnitTests <- testSpec "Parser.hs" parserSpec
    return $
        testGroup "Unit"
                  [ benchmarkUnitTests, goParserUnitTests, parserUnitTests ]

propertyTests :: TestTree
propertyTests = testGroup "Property" [ parserProps ]
