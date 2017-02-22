{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Blockchain.AnalyzeSpec
  ( spec
  ) where

import Blockchain.Analyze
import Blockchain.ExtWord
import Blockchain.VM.Opcodes
import Data.ByteString as DB
import Data.List as DL
import Data.List.Extra as DLE
import Legacy.Haskoin.V0102.Network.Haskoin.Crypto.BigWord
import SpecCommon
import Test.Hspec

showOps :: [(Word256, Operation)] -> [String]
showOps [] = []
showOps ((lineNo, op):t) = show (getBigWordInteger lineNo, op) : showOps t

spec :: Spec
spec = do
  describe "decompile" $
    do it "works" $
         do let decompiled1 = show $ showOps $ decompileHexString hexcode1
            DL.take 60 decompiled1 `shouldBe`
              "[\"(2,PUSH [96])\",\"(4,PUSH [64])\",\"(5,MSTORE)\",\"(7,PUSH [2])\""
            DLE.takeEnd 60 decompiled1 `shouldBe`
              "(6990,JUMP)\",\"(6991,JUMPDEST)\",\"(6992,SWAP1)\",\"(6993,JUMP)\"]"
