{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Blockchain.Analyze.CfgAugWithTopNPassSpec
  ( spec
  ) where

import Blockchain.Analyze
import Blockchain.Analyze.CfgAugWithTopNPass
import SpecCommon
import Test.Hspec

spec :: Spec
spec = do
  describe "doCfgAugWithTopNPass" $
    do it "works for hexcode1" $
         do let decompiled@((loc, _):_) = decompileHexString hexcode1
                result =
                  unWordLabelMapM $
                  do contract <- evmOps2HplContract decompiled
                     show <$> doCfgAugWithTopNPass contract
            length result `shouldBe` 4769
       it "works for hexcode2" $
         do let decompiled@((loc, _):_) = decompileHexString hexcode2
                result =
                  unWordLabelMapM $
                  do contract <- evmOps2HplContract decompiled
                     show <$> doCfgAugWithTopNPass contract
            result `shouldContain` "OC: 9: JUMPI -> [L2,L4]"
