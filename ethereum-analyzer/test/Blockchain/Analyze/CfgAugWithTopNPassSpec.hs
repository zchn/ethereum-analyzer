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
         do let result =
                  unWordLabelMapM $
                     show <$> doCfgAugWithTopNPass hexcode1
            length result `shouldBe` 4815
       it "works for hexcode2" $
         do let result =
                  unWordLabelMapM $
                   show <$> doCfgAugWithTopNPass hexcode2
            result `shouldContain` "OC: 9: JUMPI -> [L2,L4]"
