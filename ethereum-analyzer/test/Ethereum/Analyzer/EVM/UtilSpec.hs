{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module Ethereum.Analyzer.EVM.UtilSpec
  ( spec
  ) where

import Protolude hiding (show)

import Data.Text
import Ethereum.Analyzer.EVM
import Ethereum.Analyzer.TestData.Basic
import Ethereum.Analyzer.Util
import Test.Hspec

spec :: Spec
spec =
  describe "toDotText" $ do
    it "shows HplCfg's dot graph" $ do
      let disasmd = disasm hexstring2
      (unpack . unWordLabelMapM)
        (toDotText . ctorOf <$> evmOps2HplContract disasmd) `shouldBe`
        toS expectedHexString2RawDot
    it "shows HplCfg after CfgAugmentPass" $ do
      let disasmd@((_, _):_) = disasm hexstring2
          result =
            (unpack . unWordLabelMapM) $ do
              contract <- evmOps2HplContract disasmd
              toDotText . ctorOf <$> doCfgAugmentPass contract
      result `shouldBe` toS expectedHexString2AugDot

-- There are more opcode in the hexstring than printed here.
expectedHexString2RawDot :: Text
expectedHexString2RawDot =
  "digraph {\n" <> "    1 [label=\"CO: L1\\l" <> "OO: 0: PUSH [96]\\l" <>
  "OO: 2: PUSH [64]\\l" <>
  "OO: 4: MSTORE\\l" <>
  "OO: 5: CALLDATASIZE\\l" <>
  "OO: 6: ISZERO\\l" <>
  "OO: 7: PUSH [39]\\l" <>
  "OC: 9: JUMPI -> [L3]\\l" <>
  "\"\n" <>
  "      ,shape=box];\n" <>
  "    3 [label=\"CO: L3\\l" <>
  "OO: 10: PUSH [224]\\l" <>
  "OO: 12: PUSH [2]\\l" <>
  "OO: 14: EXP\\l" <>
  "OO: 15: PUSH [0]\\l" <>
  "OO: 17: CALLDATALOAD\\l" <>
  "OO: 18: DIV\\l" <>
  "OO: 19: PUSH [65,192,225,181]\\l" <>
  "OO: 24: DUP2\\l" <>
  "OO: 25: EQ\\l" <>
  "OO: 26: PUSH [110]\\l" <>
  "OC: 28: JUMPI -> [L4]\\l" <>
  "\"\n" <>
  "      ,shape=box];\n" <>
  "    4 [label=\"CO: L4\\l" <>
  "OO: 29: DUP1\\l" <>
  "OO: 30: PUSH [229,34,83,129]\\l" <>
  "OO: 35: EQ\\l" <>
  "OO: 36: PUSH [150]\\l" <>
  "OC: 38: JUMPI -> [L5]\\l" <>
  "\"\n" <>
  "      ,shape=box];\n" <>
  "    5 [label=\"CO: L5\\l" <>
  "OO: 39: JUMPDEST\\l" <>
  "OO: 40: PUSH [213]\\l" <>
  "OO: 42: PUSH [0]\\l" <>
  "OO: 44: CALLVALUE\\l" <>
  "OO: 45: GT\\l" <>
  "OO: 46: ISZERO\\l" <>
  "OO: 47: PUSH [108]\\l" <>
  "OC: 49: JUMPI -> [L6]\\l" <>
  "\"\n" <>
  "      ,shape=box];\n" <>
  "    6 [label=\"CO: L6\\l" <>
  "OO: 50: CALLVALUE\\l" <>
  "OO: 51: PUSH [96]\\l" <>
  "OO: 53: SWAP1\\l" <>
  "OO: 54: DUP2\\l" <>
  "OO: 55: MSTORE\\l" <>
  "OO: 56: PUSH [88]\\l" <>
  "OO: 58: SWAP1\\l" <>
  "OO: 59: PUSH [1]\\l" <>
  "OO: 61: PUSH [160]\\l" <>
  "OO: 63: PUSH [2]\\l" <>
  "OO: 65: EXP\\l" <>
  "OO: 66: SUB\\l" <>
  "OO: 67: CALLER\\l" <>
  "OO: 68: AND\\l" <>
  "OO: 69: SWAP1\\l" <>
  "OO: 70: PUSH [144,137,8,9,198,84,241,29,110,114,162,143,166,1,73,119,10,13,17,236,108,146,49,157,108,235,43,176,164,234,26,21]\\l" <>
  "OO: 103: SWAP1\\l" <>
  "OO: 104: PUSH [32]\\l" <>
  "OO: 106: SWAP1\\l" <>
  "OC: 107: LOG3 -> [L7]\\l" <>
  "\"\n" <>
  "      ,shape=box];\n" <>
  "    7 [label=\"CO: L7\\l" <>
  "OO: 108: JUMPDEST\\l" <>
  "OC: 109: JUMP -> []\\l" <>
  "\"\n" <>
  "      ,shape=box];\n" <>
  "    1 -> 3;\n" <>
  "    3 -> 4;\n" <>
  "    4 -> 5;\n" <>
  "    5 -> 6;\n" <>
  "    6 -> 7;\n" <>
  "}"

expectedHexString2AugDot :: Text
expectedHexString2AugDot =
  "digraph {\n" <> "    1 [label=\"CO: L1\\l" <> "OO: 0: PUSH [96]\\l" <>
  "OO: 2: PUSH [64]\\l" <>
  "OO: 4: MSTORE\\l" <>
  "OO: 5: CALLDATASIZE\\l" <>
  "OO: 6: ISZERO\\l" <>
  "OO: 7: PUSH [39]\\l" <>
  "OC: 9: JUMPI -> [L3,L5]\\l" <>
  "\"\n" <>
  "      ,shape=box];\n" <>
  "    3 [label=\"CO: L3\\l" <>
  "OO: 10: PUSH [224]\\l" <>
  "OO: 12: PUSH [2]\\l" <>
  "OO: 14: EXP\\l" <>
  "OO: 15: PUSH [0]\\l" <>
  "OO: 17: CALLDATALOAD\\l" <>
  "OO: 18: DIV\\l" <>
  "OO: 19: PUSH [65,192,225,181]\\l" <>
  "OO: 24: DUP2\\l" <>
  "OO: 25: EQ\\l" <>
  "OO: 26: PUSH [110]\\l" <>
  "OC: 28: JUMPI -> [L4,L8]\\l" <>
  "\"\n" <>
  "      ,shape=box];\n" <>
  "    4 [label=\"CO: L4\\l" <>
  "OO: 29: DUP1\\l" <>
  "OO: 30: PUSH [229,34,83,129]\\l" <>
  "OO: 35: EQ\\l" <>
  "OO: 36: PUSH [150]\\l" <>
  "OC: 38: JUMPI -> [L5,L10]\\l" <>
  "\"\n" <>
  "      ,shape=box];\n" <>
  "    5 [label=\"CO: L5\\l" <>
  "OO: 39: JUMPDEST\\l" <>
  "OO: 40: PUSH [213]\\l" <>
  "OO: 42: PUSH [0]\\l" <>
  "OO: 44: CALLVALUE\\l" <>
  "OO: 45: GT\\l" <>
  "OO: 46: ISZERO\\l" <>
  "OO: 47: PUSH [108]\\l" <>
  "OC: 49: JUMPI -> [L6,L7]\\l" <>
  "\"\n" <>
  "      ,shape=box];\n" <>
  "    6 [label=\"CO: L6\\l" <>
  "OO: 50: CALLVALUE\\l" <>
  "OO: 51: PUSH [96]\\l" <>
  "OO: 53: SWAP1\\l" <>
  "OO: 54: DUP2\\l" <>
  "OO: 55: MSTORE\\l" <>
  "OO: 56: PUSH [88]\\l" <>
  "OO: 58: SWAP1\\l" <>
  "OO: 59: PUSH [1]\\l" <>
  "OO: 61: PUSH [160]\\l" <>
  "OO: 63: PUSH [2]\\l" <>
  "OO: 65: EXP\\l" <>
  "OO: 66: SUB\\l" <>
  "OO: 67: CALLER\\l" <>
  "OO: 68: AND\\l" <>
  "OO: 69: SWAP1\\l" <>
  "OO: 70: PUSH [144,137,8,9,198,84,241,29,110,114,162,143,166,1,73,119,10,13,17,236,108,146,49,157,108,235,43,176,164,234,26,21]\\l" <>
  "OO: 103: SWAP1\\l" <>
  "OO: 104: PUSH [32]\\l" <>
  "OO: 106: SWAP1\\l" <>
  "OC: 107: LOG3 -> [L7]\\l" <>
  "\"\n" <>
  "      ,shape=box];\n" <>
  "    7 [label=\"CO: L7\\l" <>
  "OO: 108: JUMPDEST\\l" <>
  "OC: 109: JUMP -> []\\l" <>
  "\"\n" <>
  "      ,shape=box];\n" <>
  "    8 [label=\"CO: L8\\l" <>
  "OO: 110: JUMPDEST\\l" <>
  "OO: 111: PUSH [213]\\l" <>
  "OO: 113: PUSH [0]\\l" <>
  "OO: 115: SLOAD\\l" <>
  "OO: 116: PUSH [1]\\l" <>
  "OO: 118: PUSH [160]\\l" <>
  "OO: 120: PUSH [2]\\l" <>
  "OO: 122: EXP\\l" <>
  "OO: 123: SUB\\l" <>
  "OO: 124: SWAP1\\l" <>
  "OO: 125: DUP2\\l" <>
  "OO: 126: AND\\l" <>
  "OO: 127: CALLER\\l" <>
  "OO: 128: SWAP2\\l" <>
  "OO: 129: SWAP1\\l" <>
  "OO: 130: SWAP2\\l" <>
  "OO: 131: AND\\l" <>
  "OO: 132: EQ\\l" <>
  "OO: 133: ISZERO\\l" <>
  "OO: 134: PUSH [108]\\l" <>
  "OC: 136: JUMPI -> [L7,L9]\\l" <>
  "\"\n" <>
  "      ,shape=box];\n" <>
  "    9 [label=\"CO: L9\\l" <>
  "OO: 137: PUSH [0]\\l" <>
  "OO: 139: SLOAD\\l" <>
  "OO: 140: PUSH [1]\\l" <>
  "OO: 142: PUSH [160]\\l" <>
  "OO: 144: PUSH [2]\\l" <>
  "OO: 146: EXP\\l" <>
  "OO: 147: SUB\\l" <>
  "OO: 148: AND\\l" <>
  "OC: 149: SUICIDE -> []\\l" <>
  "\"\n" <>
  "      ,shape=box];\n" <>
  "    10 [label=\"CO: L10\\l" <>
  "OO: 150: JUMPDEST\\l" <>
  "OO: 151: PUSH [213]\\l" <>
  "OO: 153: PUSH [0]\\l" <>
  "OO: 155: SLOAD\\l" <>
  "OO: 156: PUSH [1]\\l" <>
  "OO: 158: PUSH [160]\\l" <>
  "OO: 160: PUSH [2]\\l" <>
  "OO: 162: EXP\\l" <>
  "OO: 163: SUB\\l" <>
  "OO: 164: SWAP1\\l" <>
  "OO: 165: DUP2\\l" <>
  "OO: 166: AND\\l" <>
  "OO: 167: CALLER\\l" <>
  "OO: 168: SWAP2\\l" <>
  "OO: 169: SWAP1\\l" <>
  "OO: 170: SWAP2\\l" <>
  "OO: 171: AND\\l" <>
  "OO: 172: EQ\\l" <>
  "OO: 173: ISZERO\\l" <>
  "OO: 174: PUSH [108]\\l" <>
  "OC: 176: JUMPI -> [L7,L11]\\l" <>
  "\"\n" <>
  "       ,shape=box];\n" <>
  "    11 [label=\"CO: L11\\l" <>
  "OO: 177: PUSH [0]\\l" <>
  "OO: 179: DUP1\\l" <>
  "OO: 180: SLOAD\\l" <>
  "OO: 181: PUSH [1]\\l" <>
  "OO: 183: PUSH [160]\\l" <>
  "OO: 185: PUSH [2]\\l" <>
  "OO: 187: EXP\\l" <>
  "OO: 188: SUB\\l" <>
  "OO: 189: SWAP1\\l" <>
  "OO: 190: DUP2\\l" <>
  "OO: 191: AND\\l" <>
  "OO: 192: SWAP2\\l" <>
  "OO: 193: SWAP1\\l" <>
  "OO: 194: ADDRESS\\l" <>
  "OO: 195: AND\\l" <>
  "OO: 196: BALANCE\\l" <>
  "OO: 197: PUSH [96]\\l" <>
  "OO: 199: DUP3\\l" <>
  "OO: 200: DUP2\\l" <>
  "OO: 201: DUP2\\l" <>
  "OO: 202: DUP2\\l" <>
  "OO: 203: DUP6\\l" <>
  "OO: 204: DUP9\\l" <>
  "OO: 205: DUP4\\l" <>
  "OC: 206: CALL -> [L12]\\l" <>
  "\"\n" <>
  "       ,shape=box];\n" <>
  "    12 [label=\"CO: L12\\l" <>
  "OO: 207: POP\\l" <>
  "OO: 208: POP\\l" <>
  "OO: 209: POP\\l" <>
  "OO: 210: POP\\l" <>
  "OO: 211: POP\\l" <>
  "OC: 212: JUMP -> []\\l" <>
  "\"\n" <>
  "       ,shape=box];\n" <>
  "    1 -> 3;\n" <>
  "    1 -> 5;\n" <>
  "    3 -> 4;\n" <>
  "    3 -> 8;\n" <>
  "    4 -> 5;\n" <>
  "    4 -> 10;\n" <>
  "    5 -> 6;\n" <>
  "    5 -> 7;\n" <>
  "    6 -> 7;\n" <>
  "    8 -> 7;\n" <>
  "    8 -> 9;\n" <>
  "    10 -> 7;\n" <>
  "    10 -> 11;\n" <>
  "    11 -> 12;\n" <>
  "}"
