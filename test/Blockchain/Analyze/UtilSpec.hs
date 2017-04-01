module Blockchain.Analyze.UtilSpec
  ( spec
  ) where

import Blockchain.Analyze
import Blockchain.Analyze.CfgAugmentPass
import Blockchain.Analyze.Util
import Data.Text
import SpecCommon
import Test.Hspec

spec :: Spec
spec = do
  describe "toDotText" $
    do it "shows HplBody's dot graph" $
         do let decompiled = decompileHexString hexcode2
            unWordLabelMapM (unpack . toDotText . bodyOf . ctorOf
                             <$> (evmOps2HplContract decompiled)) `shouldBe`
              "digraph {\n" ++
              "    1 [label=\"CO: L1\\n" ++
              "OO: 0: PUSH [96]\\n" ++
              "OO: 2: PUSH [64]\\n" ++
              "OO: 4: MSTORE\\n" ++
              "OO: 5: CALLDATASIZE\\n" ++
              "OO: 6: ISZERO\\n" ++
              "OO: 7: PUSH [39]\\n" ++
              "OC: 9: JUMPI -> [L2]\\n" ++
              "\"\n" ++
              "      ,shape=box];\n" ++
              "    2 [label=\"CO: L2\\n" ++
              "OO: 10: PUSH [224]\\n" ++
              "OO: 12: PUSH [2]\\n" ++
              "OO: 14: EXP\\n" ++
              "OO: 15: PUSH [0]\\n" ++
              "OO: 17: CALLDATALOAD\\n" ++
              "OO: 18: DIV\\n" ++
              "OO: 19: PUSH [65,192,225,181]\\n" ++
              "OO: 24: DUP2\\n" ++
              "OO: 25: EQ\\n" ++
              "OO: 26: PUSH [110]\\n" ++
              "OC: 28: JUMPI -> [L3]\\n" ++
              "\"\n" ++
              "      ,shape=box];\n" ++
              "    3 [label=\"CO: L3\\n" ++
              "OO: 29: DUP1\\n" ++
              "OO: 30: PUSH [229,34,83,129]\\n" ++
              "OO: 35: EQ\\n" ++
              "OO: 36: PUSH [150]\\n" ++
              "OC: 38: JUMPI -> [L4]\\n" ++
              "\"\n" ++
              "      ,shape=box];\n" ++
              "    4 [label=\"CO: L4\\n" ++
              "OO: 39: JUMPDEST\\n" ++
              "OO: 40: PUSH [213]\\n" ++
              "OO: 42: PUSH [0]\\n" ++
              "OO: 44: CALLVALUE\\n" ++
              "OO: 45: GT\\n" ++
              "OO: 46: ISZERO\\n" ++
              "OO: 47: PUSH [108]\\n" ++
              "OC: 49: JUMPI -> [L5]\\n" ++
              "\"\n" ++
              "      ,shape=box];\n" ++
              "    5 [label=\"CO: L5\\n" ++
              "OO: 50: CALLVALUE\\n" ++
              "OO: 51: PUSH [96]\\n" ++
              "OO: 53: SWAP1\\n" ++
              "OO: 54: DUP2\\n" ++
              "OO: 55: MSTORE\\n" ++
              "OO: 56: PUSH [88]\\n" ++
              "OO: 58: SWAP1\\n" ++
              "OO: 59: PUSH [1]\\n" ++
              "OO: 61: PUSH [160]\\n" ++
              "OO: 63: PUSH [2]\\n" ++
              "OO: 65: EXP\\n" ++
              "OO: 66: SUB\\n" ++
              "OO: 67: CALLER\\n" ++
              "OO: 68: AND\\n" ++
              "OO: 69: SWAP1\\n" ++
              "OO: 70: PUSH [144,137,8,9,198,84,241,29,110,114,162,143,166,1,73,119,10,13,17,236,108,146,49,157,108,235,43,176,164,234,26,21]\\n" ++
              "OO: 103: SWAP1\\n" ++
              "OO: 104: PUSH [32]\\n" ++
              "OO: 106: SWAP1\\n" ++
              "OC: 107: LOG3 -> [L6]\\n" ++
              "\"\n" ++
              "      ,shape=box];\n" ++
              "    6 [label=\"CO: L6\\n" ++
              "OO: 108: JUMPDEST\\n" ++
              "OC: 109: JUMP -> []\\n" ++
              "\"\n" ++
              "      ,shape=box];\n" ++
              "    7 [label=\"CO: L7\\n" ++
              "OO: 110: JUMPDEST\\n" ++
              "OO: 111: PUSH [213]\\n" ++
              "OO: 113: PUSH [0]\\n" ++
              "OO: 115: SLOAD\\n" ++
              "OO: 116: PUSH [1]\\n" ++
              "OO: 118: PUSH [160]\\n" ++
              "OO: 120: PUSH [2]\\n" ++
              "OO: 122: EXP\\n" ++
              "OO: 123: SUB\\n" ++
              "OO: 124: SWAP1\\n" ++
              "OO: 125: DUP2\\n" ++
              "OO: 126: AND\\n" ++
              "OO: 127: CALLER\\n" ++
              "OO: 128: SWAP2\\n" ++
              "OO: 129: SWAP1\\n" ++
              "OO: 130: SWAP2\\n" ++
              "OO: 131: AND\\n" ++
              "OO: 132: EQ\\n" ++
              "OO: 133: ISZERO\\n" ++
              "OO: 134: PUSH [108]\\n" ++
              "OC: 136: JUMPI -> [L8]\\n" ++
              "\"\n" ++
              "      ,shape=box];\n" ++
              "    8 [label=\"CO: L8\\n" ++
              "OO: 137: PUSH [0]\\n" ++
              "OO: 139: SLOAD\\n" ++
              "OO: 140: PUSH [1]\\n" ++
              "OO: 142: PUSH [160]\\n" ++
              "OO: 144: PUSH [2]\\n" ++
              "OO: 146: EXP\\n" ++
              "OO: 147: SUB\\n" ++
              "OO: 148: AND\\n" ++
              "OC: 149: SUICIDE -> []\\n" ++
              "\"\n" ++
              "      ,shape=box];\n" ++
              "    9 [label=\"CO: L9\\n" ++
              "OO: 150: JUMPDEST\\n" ++
              "OO: 151: PUSH [213]\\n" ++
              "OO: 153: PUSH [0]\\n" ++
              "OO: 155: SLOAD\\n" ++
              "OO: 156: PUSH [1]\\n" ++
              "OO: 158: PUSH [160]\\n" ++
              "OO: 160: PUSH [2]\\n" ++
              "OO: 162: EXP\\n" ++
              "OO: 163: SUB\\n" ++
              "OO: 164: SWAP1\\n" ++
              "OO: 165: DUP2\\n" ++
              "OO: 166: AND\\n" ++
              "OO: 167: CALLER\\n" ++
              "OO: 168: SWAP2\\n" ++
              "OO: 169: SWAP1\\n" ++
              "OO: 170: SWAP2\\n" ++
              "OO: 171: AND\\n" ++
              "OO: 172: EQ\\n" ++
              "OO: 173: ISZERO\\n" ++
              "OO: 174: PUSH [108]\\n" ++
              "OC: 176: JUMPI -> [L10]\\n" ++
              "\"\n" ++
              "      ,shape=box];\n" ++
              "    10 [label=\"CO: L10\\n" ++
              "OO: 177: PUSH [0]\\n" ++
              "OO: 179: DUP1\\n" ++
              "OO: 180: SLOAD\\n" ++
              "OO: 181: PUSH [1]\\n" ++
              "OO: 183: PUSH [160]\\n" ++
              "OO: 185: PUSH [2]\\n" ++
              "OO: 187: EXP\\n" ++
              "OO: 188: SUB\\n" ++
              "OO: 189: SWAP1\\n" ++
              "OO: 190: DUP2\\n" ++
              "OO: 191: AND\\n" ++
              "OO: 192: SWAP2\\n" ++
              "OO: 193: SWAP1\\n" ++
              "OO: 194: ADDRESS\\n" ++
              "OO: 195: AND\\n" ++
              "OO: 196: BALANCE\\n" ++
              "OO: 197: PUSH [96]\\n" ++
              "OO: 199: DUP3\\n" ++
              "OO: 200: DUP2\\n" ++
              "OO: 201: DUP2\\n" ++
              "OO: 202: DUP2\\n" ++
              "OO: 203: DUP6\\n" ++
              "OO: 204: DUP9\\n" ++
              "OO: 205: DUP4\\n" ++
              "OC: 206: CALL -> [L11]\\n" ++
              "\"\n" ++
              "       ,shape=box];\n" ++
              "    11 [label=\"CO: L11\\n" ++
              "OO: 207: POP\\n" ++
              "OO: 208: POP\\n" ++
              "OO: 209: POP\\n" ++
              "OO: 210: POP\\n" ++
              "OO: 211: POP\\n" ++
              "OC: 212: JUMP -> []\\n" ++
              "\"\n" ++
              "       ,shape=box];\n" ++
              "    12 [label=\"CO: L12\\n" ++
              "OO: 213: JUMPDEST\\n" ++
              "OC: 214: STOP -> []\\n" ++
              "\"\n" ++
              "       ,shape=box];\n" ++
              "    1 -> 2;\n" ++
              "    2 -> 3;\n" ++
              "    3 -> 4;\n" ++
              "    4 -> 5;\n" ++
              "    5 -> 6;\n" ++
              "    7 -> 8;\n" ++ "    9 -> 10;\n" ++ "    10 -> 11;\n" ++ "}"
       it "shows HplBody after CfgAugmentPass" $
         do let decompiled@((loc, _):_) = decompileHexString hexcode2
                result =
                  unWordLabelMapM $
                  do contract <- evmOps2HplContract decompiled
                     unpack . toDotText . bodyOf . ctorOf
                       <$> doCfgAugmentPass contract
            result `shouldBe` "digraph {\n" ++
              "    1 [label=\"CO: L1\\n" ++
              "OO: 0: PUSH [96]\\n" ++
              "OO: 2: PUSH [64]\\n" ++
              "OO: 4: MSTORE\\n" ++
              "OO: 5: CALLDATASIZE\\n" ++
              "OO: 6: ISZERO\\n" ++
              "OO: 7: PUSH [39]\\n" ++
              "OC: 9: JUMPI -> [L2,L4]\\n" ++
              "\"\n" ++
              "      ,shape=box];\n" ++
              "    2 [label=\"CO: L2\\n" ++
              "OO: 10: PUSH [224]\\n" ++
              "OO: 12: PUSH [2]\\n" ++
              "OO: 14: EXP\\n" ++
              "OO: 15: PUSH [0]\\n" ++
              "OO: 17: CALLDATALOAD\\n" ++
              "OO: 18: DIV\\n" ++
              "OO: 19: PUSH [65,192,225,181]\\n" ++
              "OO: 24: DUP2\\n" ++
              "OO: 25: EQ\\n" ++
              "OO: 26: PUSH [110]\\n" ++
              "OC: 28: JUMPI -> [L3,L7]\\n" ++
              "\"\n" ++
              "      ,shape=box];\n" ++
              "    3 [label=\"CO: L3\\n" ++
              "OO: 29: DUP1\\n" ++
              "OO: 30: PUSH [229,34,83,129]\\n" ++
              "OO: 35: EQ\\n" ++
              "OO: 36: PUSH [150]\\n" ++
              "OC: 38: JUMPI -> [L4,L9]\\n" ++
              "\"\n" ++
              "      ,shape=box];\n" ++
              "    4 [label=\"CO: L4\\n" ++
              "OO: 39: JUMPDEST\\n" ++
              "OO: 40: PUSH [213]\\n" ++
              "OO: 42: PUSH [0]\\n" ++
              "OO: 44: CALLVALUE\\n" ++
              "OO: 45: GT\\n" ++
              "OO: 46: ISZERO\\n" ++
              "OO: 47: PUSH [108]\\n" ++
              "OC: 49: JUMPI -> [L5,L6]\\n" ++
              "\"\n" ++
              "      ,shape=box];\n" ++
              "    5 [label=\"CO: L5\\n" ++
              "OO: 50: CALLVALUE\\n" ++
              "OO: 51: PUSH [96]\\n" ++
              "OO: 53: SWAP1\\n" ++
              "OO: 54: DUP2\\n" ++
              "OO: 55: MSTORE\\n" ++
              "OO: 56: PUSH [88]\\n" ++
              "OO: 58: SWAP1\\n" ++
              "OO: 59: PUSH [1]\\n" ++
              "OO: 61: PUSH [160]\\n" ++
              "OO: 63: PUSH [2]\\n" ++
              "OO: 65: EXP\\n" ++
              "OO: 66: SUB\\n" ++
              "OO: 67: CALLER\\n" ++
              "OO: 68: AND\\n" ++
              "OO: 69: SWAP1\\n" ++
              "OO: 70: PUSH [144,137,8,9,198,84,241,29,110,114,162,143,166,1,73,119,10,13,17,236,108,146,49,157,108,235,43,176,164,234,26,21]\\n" ++
              "OO: 103: SWAP1\\n" ++
              "OO: 104: PUSH [32]\\n" ++
              "OO: 106: SWAP1\\n" ++
              "OC: 107: LOG3 -> [L6]\\n" ++
              "\"\n" ++
              "      ,shape=box];\n" ++
              "    6 [label=\"CO: L6\\n" ++
              "OO: 108: JUMPDEST\\n" ++
              "OC: 109: JUMP -> []\\n" ++
              "\"\n" ++
              "      ,shape=box];\n" ++
              "    7 [label=\"CO: L7\\n" ++
              "OO: 110: JUMPDEST\\n" ++
              "OO: 111: PUSH [213]\\n" ++
              "OO: 113: PUSH [0]\\n" ++
              "OO: 115: SLOAD\\n" ++
              "OO: 116: PUSH [1]\\n" ++
              "OO: 118: PUSH [160]\\n" ++
              "OO: 120: PUSH [2]\\n" ++
              "OO: 122: EXP\\n" ++
              "OO: 123: SUB\\n" ++
              "OO: 124: SWAP1\\n" ++
              "OO: 125: DUP2\\n" ++
              "OO: 126: AND\\n" ++
              "OO: 127: CALLER\\n" ++
              "OO: 128: SWAP2\\n" ++
              "OO: 129: SWAP1\\n" ++
              "OO: 130: SWAP2\\n" ++
              "OO: 131: AND\\n" ++
              "OO: 132: EQ\\n" ++
              "OO: 133: ISZERO\\n" ++
              "OO: 134: PUSH [108]\\n" ++
              "OC: 136: JUMPI -> [L6,L8]\\n" ++
              "\"\n" ++
              "      ,shape=box];\n" ++
              "    8 [label=\"CO: L8\\n" ++
              "OO: 137: PUSH [0]\\n" ++
              "OO: 139: SLOAD\\n" ++
              "OO: 140: PUSH [1]\\n" ++
              "OO: 142: PUSH [160]\\n" ++
              "OO: 144: PUSH [2]\\n" ++
              "OO: 146: EXP\\n" ++
              "OO: 147: SUB\\n" ++
              "OO: 148: AND\\n" ++
              "OC: 149: SUICIDE -> []\\n" ++
              "\"\n" ++
              "      ,shape=box];\n" ++
              "    9 [label=\"CO: L9\\n" ++
              "OO: 150: JUMPDEST\\n" ++
              "OO: 151: PUSH [213]\\n" ++
              "OO: 153: PUSH [0]\\n" ++
              "OO: 155: SLOAD\\n" ++
              "OO: 156: PUSH [1]\\n" ++
              "OO: 158: PUSH [160]\\n" ++
              "OO: 160: PUSH [2]\\n" ++
              "OO: 162: EXP\\n" ++
              "OO: 163: SUB\\n" ++
              "OO: 164: SWAP1\\n" ++
              "OO: 165: DUP2\\n" ++
              "OO: 166: AND\\n" ++
              "OO: 167: CALLER\\n" ++
              "OO: 168: SWAP2\\n" ++
              "OO: 169: SWAP1\\n" ++
              "OO: 170: SWAP2\\n" ++
              "OO: 171: AND\\n" ++
              "OO: 172: EQ\\n" ++
              "OO: 173: ISZERO\\n" ++
              "OO: 174: PUSH [108]\\n" ++
              "OC: 176: JUMPI -> [L6,L10]\\n" ++
              "\"\n" ++
              "      ,shape=box];\n" ++
              "    10 [label=\"CO: L10\\n" ++
              "OO: 177: PUSH [0]\\n" ++
              "OO: 179: DUP1\\n" ++
              "OO: 180: SLOAD\\n" ++
              "OO: 181: PUSH [1]\\n" ++
              "OO: 183: PUSH [160]\\n" ++
              "OO: 185: PUSH [2]\\n" ++
              "OO: 187: EXP\\n" ++
              "OO: 188: SUB\\n" ++
              "OO: 189: SWAP1\\n" ++
              "OO: 190: DUP2\\n" ++
              "OO: 191: AND\\n" ++
              "OO: 192: SWAP2\\n" ++
              "OO: 193: SWAP1\\n" ++
              "OO: 194: ADDRESS\\n" ++
              "OO: 195: AND\\n" ++
              "OO: 196: BALANCE\\n" ++
              "OO: 197: PUSH [96]\\n" ++
              "OO: 199: DUP3\\n" ++
              "OO: 200: DUP2\\n" ++
              "OO: 201: DUP2\\n" ++
              "OO: 202: DUP2\\n" ++
              "OO: 203: DUP6\\n" ++
              "OO: 204: DUP9\\n" ++
              "OO: 205: DUP4\\n" ++
              "OC: 206: CALL -> [L11]\\n" ++
              "\"\n" ++
              "       ,shape=box];\n" ++
              "    11 [label=\"CO: L11\\n" ++
              "OO: 207: POP\\n" ++
              "OO: 208: POP\\n" ++
              "OO: 209: POP\\n" ++
              "OO: 210: POP\\n" ++
              "OO: 211: POP\\n" ++
              "OC: 212: JUMP -> []\\n" ++
              "\"\n" ++
              "       ,shape=box];\n" ++
              "    1 -> 2;\n" ++
              "    1 -> 4;\n" ++
              "    2 -> 3;\n" ++
              "    2 -> 7;\n" ++
              "    3 -> 4;\n" ++
              "    3 -> 9;\n" ++
              "    4 -> 5;\n" ++
              "    4 -> 6;\n" ++
              "    5 -> 6;\n" ++
              "    7 -> 6;\n" ++
              "    7 -> 8;\n" ++
              "    9 -> 6;\n" ++ "    9 -> 10;\n" ++ "    10 -> 11;\n" ++ "}"
