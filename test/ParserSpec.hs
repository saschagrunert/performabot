-- | The parser tests
--
-- @since 0.1.0
module ParserSpec ( parserProps, parserSpec ) where

import           Data.Either           ( isLeft )

import           Parser                ( State(Init), double, integer )

import           Test.Tasty            ( TestTree, testGroup )
import           Test.Tasty.Hspec
                 ( Spec, it, parallel, shouldBe, shouldSatisfy )
import           Test.Tasty.QuickCheck ( getPositive, testProperty )

import           Text.Megaparsec       ( runParser )

-- Parser.hs related unit tests
parserSpec :: Spec
parserSpec = parallel $ do
    it "should succeed to show the Init State" $ show Init `shouldBe` "Init"

    it "should succeed to parse an integer '100'" $
        runParser integer "" "100" `shouldBe` Right 100

    it "should succeed to parse an integer '123'" $
        runParser integer "" "123" `shouldBe` Right 123

    it "should succeed to parse an integer with space" $
        runParser integer "" "321    " `shouldBe` Right 321

    it "should fail to parse an integer with empty input" $
        runParser integer "" "" `shouldSatisfy` isLeft

    it "should fail to parse an integer with wrong input" $
        runParser integer "" "wrong" `shouldSatisfy` isLeft

    it "should succeed to parse a double '100.5'" $
        runParser double "" "100.5" `shouldBe` Right 100.5

    it "should succeed to parse a double '123.123'" $
        runParser double "" "123.123" `shouldBe` Right 123.123

    it "should succeed to parse a double with space" $
        runParser double "" "321.001    " `shouldBe` Right 321.001

    it "should fail to parse a double with empty input" $
        runParser double "" "" `shouldSatisfy` isLeft

    it "should fail to parse a double with wrong input" $
        runParser double "" "wrong" `shouldSatisfy` isLeft

parserProps :: TestTree
parserProps = testGroup "Parser.hs"
                        [ testProperty "integer" $ \a -> do
                              let i = getPositive a
                              runParser integer "" (show i) == Right i
                        , testProperty "double" $ \a -> do
                              let i = getPositive a
                              runParser double "" (show i) == Right i
                        ]

