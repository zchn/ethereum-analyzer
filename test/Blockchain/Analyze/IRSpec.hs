module Blockchain.Analyze.IRSpec
  ( spec
  ) where

import Blockchain.Analyze
import Compiler.Hoopl
import SpecCommon
import Test.Hspec

spec :: Spec
spec = do
  describe "e2h" $
    do it "works for hexcode1" $
         do let decompiled = decompileHexString hexcode1
            unWordLabelMapM (mapSize <$> (evmOps2HplBody decompiled)) `shouldBe`
              243
       it "works for hexcode2" $
         do let decompiled = decompileHexString hexcode2
            unWordLabelMapM (mapSize <$> (evmOps2HplBody decompiled)) `shouldBe`
              11
       it "shows HplBody" $
         do let decompiled = decompileHexString hexcode2
            unWordLabelMapM (show <$> (evmOps2HplBody decompiled)) `shouldBe`
              "LM (UM (fromList [(1," ++
              "(CO: L1," ++
              "[OO: 0: PUSH [96]," ++
              "OO: 2: PUSH [64]," ++
              "OO: 4: MSTORE," ++
              "OO: 5: CALLDATASIZE," ++
              "OO: 6: ISZERO," ++
              "OO: 7: PUSH [39]]," ++
              "OC: 9: JUMPI -> [L2]))," ++
              "(2," ++
              "(CO: L2," ++
              "[OO: 10: PUSH [224]," ++
              "OO: 12: PUSH [2]," ++
              "OO: 14: EXP," ++
              "OO: 15: PUSH [0]," ++
              "OO: 17: CALLDATALOAD," ++
              "OO: 18: DIV," ++
              "OO: 19: PUSH [65," ++
              "192," ++
              "225," ++
              "181]," ++
              "OO: 24: DUP2," ++
              "OO: 25: EQ," ++
              "OO: 26: PUSH [110]]," ++
              "OC: 28: JUMPI -> [L3]))," ++
              "(3," ++
              "(CO: L3," ++
              "[OO: 29: DUP1," ++
              "OO: 30: PUSH [229," ++
              "34," ++
              "83," ++
              "129]," ++
              "OO: 35: EQ," ++
              "OO: 36: PUSH [150]]," ++
              "OC: 38: JUMPI -> [L4]))," ++
              "(4," ++
              "(CO: L4," ++
              "[OO: 39: JUMPDEST," ++
              "OO: 40: PUSH [213]," ++
              "OO: 42: PUSH [0]," ++
              "OO: 44: CALLVALUE," ++
              "OO: 45: GT," ++
              "OO: 46: ISZERO," ++
              "OO: 47: PUSH [108]]," ++
              "OC: 49: JUMPI -> [L5]))," ++
              "(5," ++
              "(CO: L5," ++
              "[OO: 50: CALLVALUE," ++
              "OO: 51: PUSH [96]," ++
              "OO: 53: SWAP1," ++
              "OO: 54: DUP2," ++
              "OO: 55: MSTORE," ++
              "OO: 56: PUSH [88]," ++
              "OO: 58: SWAP1," ++
              "OO: 59: PUSH [1]," ++
              "OO: 61: PUSH [160]," ++
              "OO: 63: PUSH [2]," ++
              "OO: 65: EXP," ++
              "OO: 66: SUB," ++
              "OO: 67: CALLER," ++
              "OO: 68: AND," ++
              "OO: 69: SWAP1," ++
              "OO: 70: PUSH [144," ++
              "137," ++
              "8," ++
              "9," ++
              "198," ++
              "84," ++
              "241," ++
              "29," ++
              "110," ++
              "114," ++
              "162," ++
              "143," ++
              "166," ++
              "1," ++
              "73," ++
              "119," ++
              "10," ++
              "13," ++
              "17," ++
              "236," ++
              "108," ++
              "146," ++
              "49," ++
              "157," ++
              "108," ++
              "235," ++
              "43," ++
              "176," ++
              "164," ++
              "234," ++
              "26," ++
              "21]," ++
              "OO: 103: SWAP1," ++
              "OO: 104: PUSH [32]," ++
              "OO: 106: SWAP1," ++
              "OO: 107: LOG3," ++
              "OO: 108: JUMPDEST]," ++
              "OC: 109: JUMP -> []))," ++
              "(6," ++
              "(CO: L6," ++
              "[OO: 110: JUMPDEST," ++
              "OO: 111: PUSH [213]," ++
              "OO: 113: PUSH [0]," ++
              "OO: 115: SLOAD," ++
              "OO: 116: PUSH [1]," ++
              "OO: 118: PUSH [160]," ++
              "OO: 120: PUSH [2]," ++
              "OO: 122: EXP," ++
              "OO: 123: SUB," ++
              "OO: 124: SWAP1," ++
              "OO: 125: DUP2," ++
              "OO: 126: AND," ++
              "OO: 127: CALLER," ++
              "OO: 128: SWAP2," ++
              "OO: 129: SWAP1," ++
              "OO: 130: SWAP2," ++
              "OO: 131: AND," ++
              "OO: 132: EQ," ++
              "OO: 133: ISZERO," ++
              "OO: 134: PUSH [108]]," ++
              "OC: 136: JUMPI -> [L7]))," ++
              "(7," ++
              "(CO: L7," ++
              "[OO: 137: PUSH [0]," ++
              "OO: 139: SLOAD," ++
              "OO: 140: PUSH [1]," ++
              "OO: 142: PUSH [160]," ++
              "OO: 144: PUSH [2]," ++
              "OO: 146: EXP," ++
              "OO: 147: SUB," ++
              "OO: 148: AND]," ++
              "OC: 149: SUICIDE -> []))," ++
              "(8," ++
              "(CO: L8," ++
              "[OO: 150: JUMPDEST," ++
              "OO: 151: PUSH [213]," ++
              "OO: 153: PUSH [0]," ++
              "OO: 155: SLOAD," ++
              "OO: 156: PUSH [1]," ++
              "OO: 158: PUSH [160]," ++
              "OO: 160: PUSH [2]," ++
              "OO: 162: EXP," ++
              "OO: 163: SUB," ++
              "OO: 164: SWAP1," ++
              "OO: 165: DUP2," ++
              "OO: 166: AND," ++
              "OO: 167: CALLER," ++
              "OO: 168: SWAP2," ++
              "OO: 169: SWAP1," ++
              "OO: 170: SWAP2," ++
              "OO: 171: AND," ++
              "OO: 172: EQ," ++
              "OO: 173: ISZERO," ++
              "OO: 174: PUSH [108]]," ++
              "OC: 176: JUMPI -> [L9]))," ++
              "(9," ++
              "(CO: L9," ++
              "[OO: 177: PUSH [0]," ++
              "OO: 179: DUP1," ++
              "OO: 180: SLOAD," ++
              "OO: 181: PUSH [1]," ++
              "OO: 183: PUSH [160]," ++
              "OO: 185: PUSH [2]," ++
              "OO: 187: EXP," ++
              "OO: 188: SUB," ++
              "OO: 189: SWAP1," ++
              "OO: 190: DUP2," ++
              "OO: 191: AND," ++
              "OO: 192: SWAP2," ++
              "OO: 193: SWAP1," ++
              "OO: 194: ADDRESS," ++
              "OO: 195: AND," ++
              "OO: 196: BALANCE," ++
              "OO: 197: PUSH [96]," ++
              "OO: 199: DUP3," ++
              "OO: 200: DUP2," ++
              "OO: 201: DUP2," ++
              "OO: 202: DUP2," ++
              "OO: 203: DUP6," ++
              "OO: 204: DUP9," ++
              "OO: 205: DUP4]," ++
              "OC: 206: CALL -> [L10]))," ++
              "(10," ++
              "(CO: L10," ++
              "[OO: 207: POP," ++
              "OO: 208: POP," ++
              "OO: 209: POP," ++
              "OO: 210: POP," ++
              "OO: 211: POP]," ++
              "OC: 212: JUMP -> []))," ++
              "(11," ++
              "(CO: L11," ++
              "[OO: 213: JUMPDEST]," ++ "OC: 214: STOP -> []))]))"
