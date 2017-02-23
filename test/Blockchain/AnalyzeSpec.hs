{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Blockchain.AnalyzeSpec
  ( spec
  ) where

import Blockchain.Analyze
import Blockchain.ExtWord
import Blockchain.VM.Opcodes
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
    do it "works for hexcode1" $
         do let decompiled1 = show $ showOps $ decompileHexString hexcode1
            DL.take 60 decompiled1 `shouldBe`
              "[\"(2,PUSH [96])\",\"(4,PUSH [64])\",\"(5,MSTORE)\",\"(7,PUSH [2])\""
            DLE.takeEnd 60 decompiled1 `shouldBe`
              "(6990,JUMP)\",\"(6991,JUMPDEST)\",\"(6992,SWAP1)\",\"(6993,JUMP)\"]"
       it "works for hexcode2" $
         do let decompiled2 = show $ showOps $ decompileHexString hexcode2
            decompiled2 `shouldBe`
              "[\"(2,PUSH [96])\",\"(4,PUSH [64])\",\"(5,MSTORE)\",\"(6,CALLDATASIZE)\",\"(7,ISZERO)\",\"(9,PUSH [39])\",\"(10,JUMPI)\",\"(12,PUSH [224])\",\"(14,PUSH [2])\",\"(15,EXP)\",\"(17,PUSH [0])\",\"(18,CALLDATALOAD)\",\"(19,DIV)\",\"(24,PUSH [65,192,225,181])\",\"(25,DUP2)\",\"(26,EQ)\",\"(28,PUSH [110])\",\"(29,JUMPI)\",\"(30,DUP1)\",\"(35,PUSH [229,34,83,129])\",\"(36,EQ)\",\"(38,PUSH [150])\",\"(39,JUMPI)\",\"(40,JUMPDEST)\",\"(42,PUSH [213])\",\"(44,PUSH [0])\",\"(45,CALLVALUE)\",\"(46,GT)\",\"(47,ISZERO)\",\"(49,PUSH [108])\",\"(50,JUMPI)\",\"(51,CALLVALUE)\",\"(53,PUSH [96])\",\"(54,SWAP1)\",\"(55,DUP2)\",\"(56,MSTORE)\",\"(58,PUSH [88])\",\"(59,SWAP1)\",\"(61,PUSH [1])\",\"(63,PUSH [160])\",\"(65,PUSH [2])\",\"(66,EXP)\",\"(67,SUB)\",\"(68,CALLER)\",\"(69,AND)\",\"(70,SWAP1)\",\"(103,PUSH [144,137,8,9,198,84,241,29,110,114,162,143,166,1,73,119,10,13,17,236,108,146,49,157,108,235,43,176,164,234,26,21])\",\"(104,SWAP1)\",\"(106,PUSH [32])\",\"(107,SWAP1)\",\"(108,LOG3)\",\"(109,JUMPDEST)\",\"(110,JUMP)\",\"(111,JUMPDEST)\",\"(113,PUSH [213])\",\"(115,PUSH [0])\",\"(116,SLOAD)\",\"(118,PUSH [1])\",\"(120,PUSH [160])\",\"(122,PUSH [2])\",\"(123,EXP)\",\"(124,SUB)\",\"(125,SWAP1)\",\"(126,DUP2)\",\"(127,AND)\",\"(128,CALLER)\",\"(129,SWAP2)\",\"(130,SWAP1)\",\"(131,SWAP2)\",\"(132,AND)\",\"(133,EQ)\",\"(134,ISZERO)\",\"(136,PUSH [108])\",\"(137,JUMPI)\",\"(139,PUSH [0])\",\"(140,SLOAD)\",\"(142,PUSH [1])\",\"(144,PUSH [160])\",\"(146,PUSH [2])\",\"(147,EXP)\",\"(148,SUB)\",\"(149,AND)\",\"(150,SUICIDE)\",\"(151,JUMPDEST)\",\"(153,PUSH [213])\",\"(155,PUSH [0])\",\"(156,SLOAD)\",\"(158,PUSH [1])\",\"(160,PUSH [160])\",\"(162,PUSH [2])\",\"(163,EXP)\",\"(164,SUB)\",\"(165,SWAP1)\",\"(166,DUP2)\",\"(167,AND)\",\"(168,CALLER)\",\"(169,SWAP2)\",\"(170,SWAP1)\",\"(171,SWAP2)\",\"(172,AND)\",\"(173,EQ)\",\"(174,ISZERO)\",\"(176,PUSH [108])\",\"(177,JUMPI)\",\"(179,PUSH [0])\",\"(180,DUP1)\",\"(181,SLOAD)\",\"(183,PUSH [1])\",\"(185,PUSH [160])\",\"(187,PUSH [2])\",\"(188,EXP)\",\"(189,SUB)\",\"(190,SWAP1)\",\"(191,DUP2)\",\"(192,AND)\",\"(193,SWAP2)\",\"(194,SWAP1)\",\"(195,ADDRESS)\",\"(196,AND)\",\"(197,BALANCE)\",\"(199,PUSH [96])\",\"(200,DUP3)\",\"(201,DUP2)\",\"(202,DUP2)\",\"(203,DUP2)\",\"(204,DUP6)\",\"(205,DUP9)\",\"(206,DUP4)\",\"(207,CALL)\",\"(208,POP)\",\"(209,POP)\",\"(210,POP)\",\"(211,POP)\",\"(212,POP)\",\"(213,JUMP)\",\"(214,JUMPDEST)\",\"(215,STOP)\"]"
