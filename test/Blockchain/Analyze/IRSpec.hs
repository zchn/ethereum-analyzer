module Blockchain.Analyze.IRSpec
  ( spec
  ) where

import Blockchain.Analyze
import Blockchain.Analyze.IR
import SpecCommon
import Test.Hspec

spec :: Spec
spec = do
  describe "e2h" $
    do it "works" $
         do let decompiled = decompileHexString hexcode1
            runWordLabelMapM (hplBody2EvmOps <$> evmOps2HplBody decompiled) `shouldBe`
              decompiled
