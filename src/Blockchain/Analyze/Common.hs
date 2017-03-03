module Blockchain.Analyze.Common (varBytesToWord256) where

import Data.ByteString as DB
import Data.Word
import Blockchain.ExtWord

zero256 :: ByteString
zero256 = DB.replicate 32 0

varBytesToWord256 :: [Word8] -> Word256
varBytesToWord256 w8l =
  let extended = (zero256 `append` DB.pack w8l)
  in bytesToWord256 $ DB.unpack $ DB.drop (DB.length extended - 32) extended
