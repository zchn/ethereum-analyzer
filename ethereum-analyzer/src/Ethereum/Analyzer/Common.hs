module Ethereum.Analyzer.Common
  ( fromRight
  , varBytesToWord256
  , unexpectedPanic
  , unimplementedPanic
  ) where

import Protolude hiding (show)

import Blockchain.ExtWord
import Data.ByteString as DB
import Data.Either (Either(..))
import GHC.Show (Show(..))

zero256 :: ByteString
zero256 = DB.replicate 32 0

varBytesToWord256 :: [Word8] -> Word256
varBytesToWord256 w8l =
  let extended = (zero256 `append` DB.pack w8l)
  in bytesToWord256 $ DB.unpack $ DB.drop (DB.length extended - 32) extended

fromRight :: b -> Either a b -> b
fromRight _ (Right v) = v
fromRight v (Left _) = v

unexpectedPanic :: Show a => a -> b
unexpectedPanic n = panic . toS $ "unexpected: " ++ show n

unimplementedPanic :: Show a => a -> b
unimplementedPanic n = panic . toS $ "unimplemented: " ++ show n
