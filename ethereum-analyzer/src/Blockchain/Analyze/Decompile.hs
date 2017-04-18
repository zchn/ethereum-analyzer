module Ethereum.Analyzer.Decompile
  ( decompile
  , decompileHexString
  ) where

import Blockchain.Data.Code
import Blockchain.ExtWord
import Blockchain.Util
import Blockchain.VM.Code
import Blockchain.VM.Opcodes
import Data.ByteString
import Data.HexString

decompileHexString :: ByteString -> [(Word256, Operation)]
decompileHexString = decompileBS . toBytes . hexString

decompile :: Code -> [(Word256, Operation)]
decompile (Code bs) = decompileBS bs
decompile _ = []

decompileBS :: ByteString -> [(Word256, Operation)]
decompileBS bs =
  let hardlimit = 10000
  in decompileBSAt bs 0 hardlimit

decompileBSAt :: ByteString -> Word256 -> Int -> [(Word256, Operation)]
decompileBSAt "" _ _ = []
decompileBSAt _ _ 0 = []
decompileBSAt bs base limit =
  (base, op) : decompileBSAt (safeDrop next bs) (base + next) (limit - 1)
  where
    (op, next) = getOperationAt' bs 0
