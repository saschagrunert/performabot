-- | The main test module
--
-- @since 0.1.0
module Main ( main ) where

import           ParserGoSpec          ( parserGoSpec )

import           ParserSpec            ( parserProps, parserSpec )

import           PrettySpec            ( prettySpec )

import           ResultSpec            ( resultSpec )

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
    parserGoUnitTests <- testSpec "ParserGo.hs" parserGoSpec
    parserUnitTests <- testSpec "Parser.hs" parserSpec
    prettyUnitTests <- testSpec "Pretty.hs" prettySpec
    resultUnitTests <- testSpec "Result.hs" resultSpec
    return $ testGroup "Unit"
                       [ parserGoUnitTests
                       , parserUnitTests
                       , prettyUnitTests
                       , resultUnitTests
                       ]

propertyTests :: TestTree
propertyTests = testGroup "Property" [ parserProps ]
