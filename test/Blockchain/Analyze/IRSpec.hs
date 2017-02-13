module Blockchain.Analyze.IRSpec (spec) where

import Blockchain.Analyze.IR
import Test.Hspec

spec :: Spec
spec = do
  describe "e2h" $ do
    it "works" $ do
      e2h ExtOp `shouldBe` (e2h $ h2e $ e2h ExtOp)
