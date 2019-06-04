-- | The parser tests
--
-- @since 0.1.0
module ParserSpec ( parserSpec ) where

import           Parser           ( parse )

import           Test.Tasty.Hspec ( Spec, it, parallel, shouldBe )

-- Parser.hs related unit tests
parserSpec :: Spec
parserSpec = parallel $ it "should succeed" $ parse "test" `shouldBe` True

