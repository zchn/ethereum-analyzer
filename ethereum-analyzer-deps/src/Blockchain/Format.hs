{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Blockchain.Format
  ( Format(..)
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC

import Blockchain.ExtWord

class Format a where
  format :: a -> String

instance Format B.ByteString where
  format x = BC.unpack (B16.encode x)

instance Format Word256 where
  format x = BC.unpack $ B16.encode $ B.pack $ word256ToBytes x
