{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Ethereum.Executable.BytecodeVisMain
    ( bytecodeVisMain
    ) where

import Control.Monad.Logger
import Data.Text
import Protolude

bytecodeVisMain :: Text -> Text -> LoggingT IO ()
bytecodeVisMain _ _ = -- hexstring dotFilepath =
  error "Unimplemented."
