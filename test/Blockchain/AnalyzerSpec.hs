module Blockchain.AnalyzerSpec (spec) where

import Blockchain.Analyzer
import Test.Hspec

spec :: Spec
spec = do
  describe "strip" $ do
    it "removes leading and trailing whitespace" $ do
      "\t  foo bar\n" `shouldBe` "foo bar"
