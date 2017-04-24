{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ethereum.Executable.BytecodeVisMain
  ( bytecodeVisMain
  ) where

import Control.Monad.Logger
import Data.String.ToString
import Data.Text (null)
import Ethereum.Analyzer.Disasm
import Ethereum.Analyzer.Util
import Protolude hiding (null)

bytecodeVisMain :: Text -> Text -> LoggingT IO ()
bytecodeVisMain hexstring dotFilepath = do
  hs <-
    if null hexstring
      then lift getLine
      else return hexstring
  let (ctorDot, dispDot) = disasmToDotText2 $ EvmHexString hs
  if null dotFilepath
    then putText dispDot
    else do
      lift $ writeFile (toString dotFilepath) dispDot
      lift $ writeFile (toString $ dotFilepath <> "ctor.dot") ctorDot
