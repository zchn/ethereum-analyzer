{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Ethereum.AnalyzerSpec
  ( spec
  ) where

import Data.List as DL
import Data.List.Extra as DLE
import Ethereum.Analyzer
import SpecCommon
import Test.Hspec

spec :: Spec
spec =
  describe "disasm" $ do
  it "works for hexstring1" $ do
    let disasmd1 = show $ showOps $ disasm hexstring1
    DL.take 60 disasmd1 `shouldBe`
      "[\"0: PUSH [96]\",\"2: PUSH [64]\",\"4: MSTORE\",\"5: PUSH [2]\",\"7:"
    DLE.takeEnd 60 disasmd1 `shouldBe`
      "]\",\"6989: JUMP\",\"6990: JUMPDEST\",\"6991: SWAP1\",\"6992: JUMP\"]"
  it "works for hexstring2" $ do
    let disasmd2 = show $ showOps $ disasm hexstring2
    disasmd2 `shouldBe` "[\"0: PUSH [96]\"," ++
      "\"2: PUSH [64]\"," ++
      "\"4: MSTORE\"," ++
      "\"5: CALLDATASIZE\"," ++
      "\"6: ISZERO\"," ++
      "\"7: PUSH [39]\"," ++
      "\"9: JUMPI\"," ++
      "\"10: PUSH [224]\"," ++
      "\"12: PUSH [2]\"," ++
      "\"14: EXP\"," ++
      "\"15: PUSH [0]\"," ++
      "\"17: CALLDATALOAD\"," ++
      "\"18: DIV\"," ++
      "\"19: PUSH [65,192,225,181]\"," ++
      "\"24: DUP2\"," ++
      "\"25: EQ\"," ++
      "\"26: PUSH [110]\"," ++
      "\"28: JUMPI\"," ++
      "\"29: DUP1\"," ++
      "\"30: PUSH [229,34,83,129]\"," ++
      "\"35: EQ\"," ++
      "\"36: PUSH [150]\"," ++
      "\"38: JUMPI\"," ++
      "\"39: JUMPDEST\"," ++
      "\"40: PUSH [213]\"," ++
      "\"42: PUSH [0]\"," ++
      "\"44: CALLVALUE\"," ++
      "\"45: GT\"," ++
      "\"46: ISZERO\"," ++
      "\"47: PUSH [108]\"," ++
      "\"49: JUMPI\"," ++
      "\"50: CALLVALUE\"," ++
      "\"51: PUSH [96]\"," ++
      "\"53: SWAP1\"," ++
      "\"54: DUP2\"," ++
      "\"55: MSTORE\"," ++
      "\"56: PUSH [88]\"," ++
      "\"58: SWAP1\"," ++
      "\"59: PUSH [1]\"," ++
      "\"61: PUSH [160]\"," ++
      "\"63: PUSH [2]\"," ++
      "\"65: EXP\"," ++
      "\"66: SUB\"," ++
      "\"67: CALLER\"," ++
      "\"68: AND\"," ++
      "\"69: SWAP1\"," ++
      "\"70: PUSH [144,137,8,9,198,84,241,29,110,114,162,143,166,1,73,119,10,13,17,236,108,146,49,157,108,235,43,176,164,234,26,21]\"," ++
      "\"103: SWAP1\"," ++
      "\"104: PUSH [32]\"," ++
      "\"106: SWAP1\"," ++
      "\"107: LOG3\"," ++
      "\"108: JUMPDEST\"," ++
      "\"109: JUMP\"," ++
      "\"110: JUMPDEST\"," ++
      "\"111: PUSH [213]\"," ++
      "\"113: PUSH [0]\"," ++
      "\"115: SLOAD\"," ++
      "\"116: PUSH [1]\"," ++
      "\"118: PUSH [160]\"," ++
      "\"120: PUSH [2]\"," ++
      "\"122: EXP\"," ++
      "\"123: SUB\"," ++
      "\"124: SWAP1\"," ++
      "\"125: DUP2\"," ++
      "\"126: AND\"," ++
      "\"127: CALLER\"," ++
      "\"128: SWAP2\"," ++
      "\"129: SWAP1\"," ++
      "\"130: SWAP2\"," ++
      "\"131: AND\"," ++
      "\"132: EQ\"," ++
      "\"133: ISZERO\"," ++
      "\"134: PUSH [108]\"," ++
      "\"136: JUMPI\"," ++
      "\"137: PUSH [0]\"," ++
      "\"139: SLOAD\"," ++
      "\"140: PUSH [1]\"," ++
      "\"142: PUSH [160]\"," ++
      "\"144: PUSH [2]\"," ++
      "\"146: EXP\"," ++
      "\"147: SUB\"," ++
      "\"148: AND\"," ++
      "\"149: SUICIDE\"," ++
      "\"150: JUMPDEST\"," ++
      "\"151: PUSH [213]\"," ++
      "\"153: PUSH [0]\"," ++
      "\"155: SLOAD\"," ++
      "\"156: PUSH [1]\"," ++
      "\"158: PUSH [160]\"," ++
      "\"160: PUSH [2]\"," ++
      "\"162: EXP\"," ++
      "\"163: SUB\"," ++
      "\"164: SWAP1\"," ++
      "\"165: DUP2\"," ++
      "\"166: AND\"," ++
      "\"167: CALLER\"," ++
      "\"168: SWAP2\"," ++
      "\"169: SWAP1\"," ++
      "\"170: SWAP2\"," ++
      "\"171: AND\"," ++
      "\"172: EQ\"," ++
      "\"173: ISZERO\"," ++
      "\"174: PUSH [108]\"," ++
      "\"176: JUMPI\"," ++
      "\"177: PUSH [0]\"," ++
      "\"179: DUP1\"," ++
      "\"180: SLOAD\"," ++
      "\"181: PUSH [1]\"," ++
      "\"183: PUSH [160]\"," ++
      "\"185: PUSH [2]\"," ++
      "\"187: EXP\"," ++
      "\"188: SUB\"," ++
      "\"189: SWAP1\"," ++
      "\"190: DUP2\"," ++
      "\"191: AND\"," ++
      "\"192: SWAP2\"," ++
      "\"193: SWAP1\"," ++
      "\"194: ADDRESS\"," ++
      "\"195: AND\"," ++
      "\"196: BALANCE\"," ++
      "\"197: PUSH [96]\"," ++
      "\"199: DUP3\"," ++
      "\"200: DUP2\"," ++
      "\"201: DUP2\"," ++
      "\"202: DUP2\"," ++
      "\"203: DUP6\"," ++
      "\"204: DUP9\"," ++
      "\"205: DUP4\"," ++
      "\"206: CALL\"," ++
      "\"207: POP\"," ++
      "\"208: POP\"," ++
      "\"209: POP\"," ++
      "\"210: POP\"," ++
      "\"211: POP\"," ++
      "\"212: JUMP\"," ++ "\"213: JUMPDEST\"," ++ "\"214: STOP\"]"
