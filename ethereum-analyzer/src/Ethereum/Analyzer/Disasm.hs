module Ethereum.Analyzer.Disasm
  ( EvmBytecode (..)
  , EvmHexString (..)
  , HasEvmBytecode
  , disasm
  ) where

import Blockchain.Data.Code
import Blockchain.ExtWord
import Blockchain.Util
import Blockchain.VM.Code
import Blockchain.VM.Opcodes
import Data.ByteString
import Data.HexString

class HasEvmBytecode a where
  evmBytecodeOf :: a -> EvmBytecode

newtype EvmBytecode = EvmBytecode { unEvmBytecode :: ByteString } deriving (Show, Eq)

newtype EvmHexString = EvmHexString { unEvmHexString :: ByteString } deriving (Show, Eq)

instance HasEvmBytecode EvmBytecode where
  evmBytecodeOf = id

instance HasEvmBytecode EvmHexString where
  evmBytecodeOf = EvmBytecode . toBytes . hexString . unEvmHexString

instance HasEvmBytecode Code where
  evmBytecodeOf (Code bs) = EvmBytecode bs
  evmBytecodeOf _ = EvmBytecode ""

disasm :: HasEvmBytecode a => a -> [(Word256, Operation)]
disasm a =
  let bs = (unEvmBytecode . evmBytecodeOf) a
      hardlimit = 10000
  in disasmBSAt bs 0 hardlimit

disasmBSAt :: ByteString -> Word256 -> Int -> [(Word256, Operation)]
disasmBSAt "" _ _ = []
disasmBSAt _ _ 0 = []
disasmBSAt bs base limit =
  (base, op) : disasmBSAt (safeDrop next bs) (base + next) (limit - 1)
  where
    (op, next) = getOperationAt' bs 0
