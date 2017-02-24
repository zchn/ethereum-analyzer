module Blockchain.Analyze.IRSpec
  ( spec
  ) where

import Blockchain.Analyze
import Compiler.Hoopl
import SpecCommon
import Test.Hspec

spec :: Spec
spec = do
  describe "e2h" $
    do it "works for hexcode1" $
         do let decompiled = decompileHexString hexcode1
            unWordLabelMapM (mapSize <$> (evmOps2HplBody decompiled)) `shouldBe`
              243
       it "works for hexcode2" $
         do let decompiled = decompileHexString hexcode2
            unWordLabelMapM (mapSize <$> (evmOps2HplBody decompiled)) `shouldBe`
              11
