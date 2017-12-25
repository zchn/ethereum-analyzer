{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, FlexibleContexts
  #-}

module Ethereum.Analyzer.EVM.CfgAugWithTopNPassSpec
  ( spec
  ) where

import Protolude hiding (show)

import Data.Text as DT
import Ethereum.Analyzer.EVM
import Ethereum.Analyzer.TestData.Basic
import Ckev.In.Text
import Test.Hspec

spec :: Spec
spec =
  describe "doCfgAugWithTopNPass" $ do
    it "works for hexstring1" $ do
      let result =
            unWordLabelMapM $ showText <$> doCfgAugWithTopNPass hexstring1
      DT.length result `shouldBe` 4876
    it "works for hexstring2" $ do
      let result =
            toS $
            unWordLabelMapM
              ((toS . showText <$> doCfgAugWithTopNPass hexstring2) :: WordLabelMapM Text)
      (result :: [Char]) `shouldContain` "OC: 9: JUMPI -> [L3,L5]"
