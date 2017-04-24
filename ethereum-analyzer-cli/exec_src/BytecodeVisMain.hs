{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts
  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Dump all contracts.
module Main
  ( main
  ) where

import Control.Monad.Logger
import Ethereum.Executable.BytecodeVisMain (bytecodeVisMain)
import HFlags
import Protolude

defineFlag
  "bytecode"
  ("" :: Text)
  "The bytecode. Will read from STDIN if empty."

defineFlag
  "outDot"
  ("" :: Text)
  "The dot file path. Will print to STDOUT if empty"

defineFlag
  "dummy"
  False
  "dummy flag for https://github.com/nilcons/hflags/issues/14"

main :: IO ()
main = do
  s <- $initHFlags "ea-bytecode-vis"
  putText $ "Flags: " <> show s
  runStdoutLoggingT (bytecodeVisMain flags_bytecode flags_outDot)
