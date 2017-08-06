{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, FlexibleContexts
  #-}

module Ethereum.Analyzer.CfgAugWithTopNPassSpec
  ( spec
  ) where

import Protolude hiding (show)

import Data.Text as DT
import Ethereum.Analyzer
import Ethereum.Analyzer.CfgAugWithTopNPass
import Ethereum.Analyzer.TestData.Basic
import GHC.Show
import Test.Hspec

spec :: Spec
spec =
  describe "doCfgAugWithTopNPass" $ do
    it "works for hexstring1" $ do
      let result =
            unWordLabelMapM $ toS . show <$> doCfgAugWithTopNPass hexstring1
      DT.length result `shouldBe` 4815
    it "works for hexstring2" $ do
      let result =
            (toS $
             unWordLabelMapM
               ((toS . show <$> doCfgAugWithTopNPass hexstring2) :: WordLabelMapM Text))
      result `shouldContain` "OC: 9: JUMPI -> [L2,L4]"
