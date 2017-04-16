{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Blockchain.ExtWord (
  Word128,
  Word160,
  Word256,
  Word512,
  word64ToBytes,
  bytesToWord64,
  word128ToBytes,
  bytesToWord128,
  word160ToBytes,
  bytesToWord160,
  word256ToBytes,
  bytesToWord256
  ) where

import Data.Binary
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Legacy.Haskoin.V0102.Network.Haskoin.Crypto.BigWord (Word128, Word160, Word256, Word512)

import Data.Ix

import Blockchain.Data.RLP


instance Ix Word256 where
    range (x, y) | x == y = [x]
    range (x, y) = x:range (x+1, y)
    index (x, y) z | z < x || z > y = error $ "Ix{Word256}.index: Index (" ++ show z ++ ") out of range ((" ++ show x ++ "," ++ show y ++ "))"
    index (x, _) z = fromIntegral $ z - x
    inRange (x, y) z | z >= x && z <= y = True
    inRange _ _ = False

instance RLPSerializable Word512 where
    rlpEncode val = RLPString $ BL.toStrict $ encode val

    rlpDecode (RLPString s) | B.length s == 64 = decode $ BL.fromStrict s
    rlpDecode x = error ("Missing case in rlp2Word512: " ++ show x)

word64ToBytes::Word64->[Word8]
word64ToBytes word = map (fromIntegral . (word `shiftR`)) [64-8, 64-16..0]

bytesToWord64::[Word8]->Word64
bytesToWord64 bytes | length bytes == 8 =
  sum $ map (\(shiftBits, byte) -> fromIntegral byte `shiftL` shiftBits) $ zip [64-8,64-16..0] bytes
bytesToWord64 _ = error "bytesToWord64 was called with the wrong number of bytes"

word128ToBytes::Word128->[Word8]
word128ToBytes word = map (fromIntegral . (word `shiftR`)) [128-8, 128-16..0]

bytesToWord128::[Word8]->Word128
bytesToWord128 bytes | length bytes == 16 =
  sum $ map (\(shiftBits, byte) -> fromIntegral byte `shiftL` shiftBits) $ zip [128-8,128-16..0] bytes
bytesToWord128 _ = error "bytesToWord128 was called with the wrong number of bytes"

word160ToBytes::Word160->[Word8]
word160ToBytes word = map (fromIntegral . (word `shiftR`)) [160-8, 160-16..0]

bytesToWord160::[Word8]->Word160
bytesToWord160 bytes | length bytes == 20 =
  sum $ map (\(shiftBits, byte) -> fromIntegral byte `shiftL` shiftBits) $ zip [160-8,160-16..0] bytes
bytesToWord160 _ = error "bytesToWord128 was called with the wrong number of bytes"

word256ToBytes::Word256->[Word8]
word256ToBytes word = map (fromIntegral . (word `shiftR`)) [256-8, 256-16..0]

instance RLPSerializable Word128 where
    rlpEncode val = RLPString $ BL.toStrict $ encode val

    rlpDecode (RLPString s) | B.null s = 0
    rlpDecode (RLPString s) | B.length s <= 16 = decode $ BL.fromStrict s
    rlpDecode x = error ("Missing case in rlp2Word128: " ++ show x)


instance RLPSerializable Word32 where
    rlpEncode val = RLPString $ BL.toStrict $ encode val

    rlpDecode (RLPString s) | B.null s = 0
    rlpDecode (RLPString s) | B.length s <= 4 = decode $ BL.fromStrict s
    rlpDecode x = error ("Missing case in rlp2Word32: " ++ show x)

instance RLPSerializable Word16 where
    rlpEncode val = RLPString $ BL.toStrict $ encode val

    rlpDecode (RLPString s) | B.null s = 0
    rlpDecode (RLPString s) | B.length s <= 2 = decode $ BL.fromStrict s
    rlpDecode x = error ("Missing case in rlp2Word16: " ++ show x)



bytesToWord256::[Word8]->Word256
bytesToWord256 bytes | length bytes == 32 =
  sum $ map (\(shiftBits, byte) -> fromIntegral byte `shiftL` shiftBits) $ zip [256-8,256-16..0] bytes
bytesToWord256 _ = error "bytesToWord256 was called with the wrong number of bytes"
