{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}

module Executable.EthAnalyzer (
  ethAnalyzer
  ) where

import Control.Monad.Logger

ethAnalyzer::LoggingT IO ()
ethAnalyzer = do
  return ()
