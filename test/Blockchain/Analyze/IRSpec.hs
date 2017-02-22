module Blockchain.Analyze.IRSpec
  ( spec
  ) where

import Blockchain.Analyze
import Blockchain.Analyze.InstCounter
import SpecCommon
import Test.Hspec

spec :: Spec
spec = do
  describe "e2h" $
    do it "works" $
         do let decompiled = decompileHexString hexcode1
            doInstCounter (evmOps2HplBody decompiled) `shouldBe` length decompiled
