{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Executable.EthAnalyzer
  ( ethAnalyzer
  ) where

import Blockchain.Jsonrpc.Client
import Control.Monad.Logger
import Data.Text as T

ethAnalyzer :: String -> Int -> LoggingT IO ()
ethAnalyzer server port = do
  $logInfo "Starting ethereum-analyzer."
  ver <- web3ClientVersion server port
  $logInfo $ T.append "version is " ver
  return ()
