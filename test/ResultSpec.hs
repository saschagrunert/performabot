-- | The result tests
--
-- @since 0.1.0
module ResultSpec ( resultSpec ) where

import           Result           ( amount, initParserStep, parseStepIO )

import           Test.Tasty.Hspec ( Spec, it, parallel, shouldBe )

-- Result.hs related unit tests
resultSpec :: Spec
resultSpec = parallel $ do
    it "should have an initParserStep amount of 0" $
        amount initParserStep `shouldBe` 0

    it "should fail to parse an empty string" $ do
        res <- parseStepIO initParserStep ""
        amount res `shouldBe` 0
