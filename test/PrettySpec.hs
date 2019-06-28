-- | The pretty tests
--
-- @since 0.1.0
module PrettySpec ( prettySpec ) where

import           Data.Time.Clock  ( getCurrentTime )

import           Model            ( Benchmark(Benchmark), Test(Test) )

import           Pretty           ( header, prettyPrint )

import           Test.Tasty.Hspec ( Spec, it, parallel, shouldContain )

-- Pretty.hs related unit tests
prettySpec :: Spec
prettySpec = parallel $ do
    it "should succeed pretty print without previous result" $ do
        let bs = [ Benchmark 0 0 "name" 1 "s" ]
            res = prettyPrint bs Nothing
        res `shouldContain` "Nothing to compare against"
        res `shouldContain` header
        res `shouldContain` "name"
        res `shouldContain` "=\n```"

    it "should succeed pretty print with previous result" $ do
        x <- getCurrentTime
        let t = Test x "" "" "" "" []
            b1 = Benchmark 0 0 "name1" 1 "s"
            b2 = Benchmark 1 2 "this is a very very long benchmark name" 2 "s"
            res = prettyPrint [ b1, b2 ] $ Just (t, [ b1 ])
        res `shouldContain` "Comparing to commit"
        res `shouldContain` header
        res `shouldContain` "name"
        res `shouldContain` "…"
        res `shouldContain` "=\n```"

