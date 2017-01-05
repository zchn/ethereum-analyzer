{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Blockchain.Analyze
  ( decompile
  ) where

import Blockchain.Data.Code
import Blockchain.VM.Opcodes

decompile :: Code -> [Operation]
decompile = error "unimplemented."
