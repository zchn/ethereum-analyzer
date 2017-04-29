{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Ethereum.Analyzer.CfgAugWithTopNPassSpec
  ( spec
  ) where

import Ethereum.Analyzer
import Ethereum.Analyzer.CfgAugWithTopNPass
import SpecCommon
import Test.Hspec

spec :: Spec
spec =
  describe "doCfgAugWithTopNPass" $ do
  it "works for hexstring1" $ do
    let result = unWordLabelMapM $ show <$> doCfgAugWithTopNPass hexstring1
    length result `shouldBe` 4815
  it "works for hexstring2" $ do
    let result = unWordLabelMapM $ show <$> doCfgAugWithTopNPass hexstring2
    result `shouldContain` "OC: 9: JUMPI -> [L2,L4]"
