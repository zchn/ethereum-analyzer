module Ethereum.Analyzer.Common
  ( varBytesToWord256
  ) where

import Data.Bits
import Data.ByteString as DB
import Data.Word
import Data.LargeWord

zero256 :: ByteString
zero256 = DB.replicate 32 0

varBytesToWord256 :: [Word8] -> Word256
varBytesToWord256 w8l =
  let extended = (zero256 `append` DB.pack w8l)
  in bytesToWord256 $ DB.unpack $ DB.drop (DB.length extended - 32) extended

class FromToWord8 a where
  fromWord8s :: [Word8] -> a
  toWord8s :: a -> [Word8]

instance FromToWord8 Word16 where
  fromWord8s [] = 0
  fromWord8s [w8] = w8
  fromWord8s [w8, w8'] = (w8 :: Word16) `shift` 8 .&. w8'
  fromWord8s (h : t) = fromWord8s t
  toWord8s w16 = [w16 `shift` -8, w16 .&. 255]
