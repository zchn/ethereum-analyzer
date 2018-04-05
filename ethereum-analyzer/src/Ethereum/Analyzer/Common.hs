{-# LANGUAGE FlexibleContexts #-}

module Ethereum.Analyzer.Common
  ( fromRight
  , s2t4Either
  , unexpectedPanic
  , unimplementedPanic
  , varBytesToWord256
  ) where

import Protolude hiding (show)

import Blockchain.ExtWord
import Ckev.In.Text
import Data.ByteString as DB
import Data.Either (Either(..))

fromRight :: b -> Either a b -> b
fromRight _ (Right v) = v
fromRight v (Left _) = v

s2t4Either
  :: StringConv s Text
  => Either s a -> Either Text a
s2t4Either (Left s) = Left $ toS s
s2t4Either (Right r) = Right r

unexpectedPanic
  :: ShowText a
  => a -> b
unexpectedPanic n = panic $ "unexpected: " <> showText n

unimplementedPanic
  :: ShowText a
  => a -> b
unimplementedPanic n = panic $ "unimplemented: " <> showText n

varBytesToWord256 :: [Word8] -> Word256
varBytesToWord256 w8l =
  let extended = (zero256 `append` DB.pack w8l)
  in bytesToWord256 $ DB.unpack $ DB.drop (DB.length extended - 32) extended

zero256 :: ByteString
zero256 = DB.replicate 32 0
