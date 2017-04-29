{-# LANGUAGE FlexibleInstances #-}

-- | The RLP module provides a framework within which serializers can be built, described in the Ethereum Yellowpaper (<http://gavwood.com/paper.pdf>).
--
-- The 'RLPObject' is an intermediate data container, whose serialization rules are well defined.  By creating code that converts from a
-- given type to an 'RLPObject', full serialization will be specified.  The 'RLPSerializable' class provides functions to do this conversion.
module Blockchain.Data.RLP
  ( RLPObject(..)
  , formatRLPObject
  , RLPSerializable(..)
  , rlpSplit
  , rlpSerialize
  , rlpDeserialize
  ) where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Internal
import Data.Word
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.Data.Util

-- | An internal representation of generic data, with no type information.
--
-- End users will not need to directly create objects of this type (an 'RLPObject' can be created using 'rlpEncode'),
-- however the designer of a new type will need to create conversion code by making their type an instance
-- of the RLPSerializable class.
data RLPObject
  = RLPScalar Word8
  | RLPString B.ByteString
  | RLPArray [RLPObject]
  deriving (Show, Eq, Ord)

-- | Converts objects to and from 'RLPObject's.
class RLPSerializable a where
  rlpDecode :: RLPObject -> a
  rlpEncode :: a -> RLPObject

instance Pretty RLPObject where
  pretty (RLPArray objects) =
    encloseSep (text "[") (text "]") (text ", ") $ pretty <$> objects
  pretty (RLPScalar n) = text $ "0x" ++ showHex n ""
  pretty (RLPString s) = text $ "0x" ++ BC.unpack (B16.encode s)

formatRLPObject :: RLPObject -> String
formatRLPObject = show . pretty

splitAtWithError :: Int -> B.ByteString -> (B.ByteString, B.ByteString)
splitAtWithError i s
  | i > B.length s = error "splitAtWithError called with n > length arr"
splitAtWithError i s = B.splitAt i s

getLength :: Int -> B.ByteString -> (Integer, B.ByteString)
getLength sizeOfLength bytes =
  ( bytes2Integer $ B.unpack $ B.take sizeOfLength bytes
  , B.drop sizeOfLength bytes)

rlpSplit :: B.ByteString -> (RLPObject, B.ByteString)
rlpSplit input =
  case B.head input of
    x
      | x >= 192 && x <= 192 + 55 ->
        let (arrayData, nextRest) =
              splitAtWithError (fromIntegral x - 192) $ B.tail input
        in (RLPArray $ getRLPObjects arrayData, nextRest)
    x
      | x >= 0xF8 && x <= 0xFF ->
        let (arrLength, restAfterLen) =
              getLength (fromIntegral x - 0xF7) $ B.tail input
            (arrayData, nextRest) =
              splitAtWithError (fromIntegral arrLength) restAfterLen
        in (RLPArray $ getRLPObjects arrayData, nextRest)
    x
      | x >= 128 && x <= 128 + 55 ->
        let (strList, nextRest) =
              splitAtWithError (fromIntegral $ x - 128) $ B.tail input
        in (RLPString strList, nextRest)
    x
      | x >= 0xB8 && x <= 0xBF ->
        let (strLength, restAfterLen) =
              getLength (fromIntegral x - 0xB7) $ B.tail input
            (strList, nextRest) =
              splitAtWithError (fromIntegral strLength) restAfterLen
        in (RLPString strList, nextRest)
    x
      | x < 128 -> (RLPScalar x, B.tail input)
    x -> error ("Missing case in rlpSplit: " ++ show x)

getRLPObjects :: ByteString -> [RLPObject]
getRLPObjects x
  | B.null x = []
getRLPObjects theData = obj : getRLPObjects rest
  where
    (obj, rest) = rlpSplit theData

int2Bytes :: Int -> [Word8]
int2Bytes val
  | val < 0x100 = map (fromIntegral . (val `shiftR`)) [0]
int2Bytes val
  | val < 0x10000 = map (fromIntegral . (val `shiftR`)) [8, 0]
int2Bytes val
  | val < 0x1000000 = map (fromIntegral . (val `shiftR`)) [16, 8, 0]
int2Bytes val
  | val < 0x100000000 = map (fromIntegral . (val `shiftR`)) [24,16 .. 0]
int2Bytes val
  | val < 0x10000000000 = map (fromIntegral . (val `shiftR`)) [32,24 .. 0]
int2Bytes _ = error "int2Bytes not defined for val >= 0x10000000000."

rlp2Bytes :: RLPObject -> [Word8]
rlp2Bytes (RLPScalar val) = [fromIntegral val]
rlp2Bytes (RLPString s)
  | B.length s <= 55 = 0x80 + fromIntegral (B.length s) : B.unpack s
rlp2Bytes (RLPString s) =
  [0xB7 + fromIntegral (length lengthAsBytes)] ++ lengthAsBytes ++ B.unpack s
  where
    lengthAsBytes = int2Bytes $ B.length s
rlp2Bytes (RLPArray innerObjects) =
  if length innerBytes <= 55
    then 0xC0 + fromIntegral (length innerBytes) : innerBytes
    else let lenBytes = int2Bytes $ length innerBytes
         in [0xF7 + fromIntegral (length lenBytes)] ++ lenBytes ++ innerBytes
  where
    innerBytes = concat $ rlp2Bytes <$> innerObjects

--TODO- Probably should just use Data.Binary's 'Binary' class for this
-- | Converts bytes to 'RLPObject's.
--
-- Full deserialization of an object can be obtained using @rlpDecode . rlpDeserialize@.
rlpDeserialize :: B.ByteString -> RLPObject
rlpDeserialize s =
  case rlpSplit s of
    (o, x)
      | B.null x -> o
    _ ->
      error
        ("parse error converting ByteString to an RLP Object: " ++
         show (B.unpack s))

-- | Converts 'RLPObject's to bytes.
--
-- Full serialization of an object can be obtained using @rlpSerialize . rlpEncode@.
rlpSerialize :: RLPObject -> B.ByteString
rlpSerialize o = B.pack $ rlp2Bytes o

instance RLPSerializable Integer where
  rlpEncode 0 = RLPString B.empty
  rlpEncode x
    | x < 128 = RLPScalar $ fromIntegral x
  rlpEncode x = RLPString $ B.pack $ integer2Bytes x
  rlpDecode (RLPScalar x) = fromIntegral x
  rlpDecode (RLPString s) = byteString2Integer s
  rlpDecode (RLPArray _) = error "rlpDecode called for Integer for array"

instance RLPSerializable String where
  rlpEncode s = rlpEncode $ BC.pack s
  rlpDecode (RLPString s) = BC.unpack s
  rlpDecode (RLPScalar n) = [w2c $ fromIntegral n]
  rlpDecode (RLPArray x) =
    error $
    "Malformed RLP in call to rlpDecode for String: RLPObject is an array: " ++
    show (pretty x)

instance RLPSerializable B.ByteString where
  rlpEncode x
    | B.length x == 1 && B.head x < 128 = RLPScalar $ B.head x
  rlpEncode s = RLPString s
  rlpDecode (RLPScalar x) = B.singleton x
  rlpDecode (RLPString s) = s
  rlpDecode x = error ("rlpDecode for ByteString not defined for: " ++ show x)
