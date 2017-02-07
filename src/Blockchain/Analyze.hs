{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Blockchain.Analyze
  ( decompile,
    decompileHexString
  ) where

import Blockchain.Data.Code
import Blockchain.Util
import Blockchain.VM.Code
import Blockchain.VM.Opcodes
import Data.ByteString
import Data.HexString

import qualified Data.ByteString.Char8 as DBC

decompileHexString :: ByteString -> [Operation]
decompileHexString = decompileBS . toBytes . hexString

decompile :: Code -> [Operation]
decompile (Code bs) = decompileBS bs
decompile _ = []

decompileBS :: ByteString -> [Operation]
decompileBS bs = let hardlimit = 10000 in decompileBSAt bs hardlimit

decompileBSAt :: ByteString -> Int -> [Operation]
decompileBSAt "" _ = []
decompileBSAt _ 0 = []
decompileBSAt bs limit =
  [op] ++ decompileBSAt (safeDrop next bs) (limit-1)
  where (op, next) = getOperationAt' bs 0
