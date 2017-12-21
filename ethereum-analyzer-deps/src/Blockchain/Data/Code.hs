{-# LANGUAGE DeriveGeneric #-}

module Blockchain.Data.Code where

import qualified Data.ByteString as B
import GHC.Generics

import Blockchain.Data.RLP

data Code
  = Code { codeBytes :: B.ByteString }
  | PrecompiledCode Int
  deriving (Show, Eq, Read, Ord, Generic)

instance RLPSerializable Code where
  rlpEncode (Code bytes) = rlpEncode bytes
  rlpEncode (PrecompiledCode _) =
    error
      "Error in call to rlpEncode for Code: Precompiled contracts can not be serialized."
  rlpDecode = Code . rlpDecode
  -- instance Format Code where
--    format Code {codeBytes=c} = B.unpack c
