module Blockchain.Analyze.IRSpec
  ( spec
  ) where

import Blockchain.Analyze.IR
import SpecCommon
import Test.Hspec

spec :: Spec
spec = do
  describe "e2h" $
    do it "works" $
         do evmOps2HplBody decompileHexString hexcode 1 `shouldBe` evmOps2HplBody $
              hplBody2Evmops $ evmOps2HplBody $ decompileHexString hexcode1
