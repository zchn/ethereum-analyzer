{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}

import Control.Monad.Logger
import HFlags

import Executable.EthAnalyzer

main :: IO ()
main = do
  _ <- $initHFlags "Ethereum Analyzer"
  flip runLoggingT printLogMsg ethAnalyzer
