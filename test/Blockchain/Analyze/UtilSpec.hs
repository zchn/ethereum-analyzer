module Blockchain.Analyze.UtilSpec
  ( spec
  ) where

import Blockchain.Analyze
import Blockchain.Analyze.CfgAugmentPass
import Data.Text.Lazy
import SpecCommon
import Test.Hspec

spec :: Spec
spec = do
  describe "toDotText" $ do
    it "shows HplBody's dot graph" $
         do let decompiled = decompileHexString hexcode2
            unWordLabelMapM (unpack . toDotText <$> (evmOps2HplBody decompiled))
              `shouldBe` ""
    it "shows HplBody after CfgAugmentPass" $ do
      let decompiled@((loc, _):_) = decompileHexString hexcode2
          result = unWordLabelMapM $ do
            entry <- labelFor loc
            body <- evmOps2HplBody decompiled
            unpack . toDotText <$> doCfgAugmentPass entry body
      result `shouldBe` ""
