{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Executable.EthAnalyzer
  ( ethAnalyzer
  ) where

import Blockchain.Jsonrpc.Client
import Blockchain.VM.Code
import Control.Monad.Logger
import Data.Text as T

ethAnalyzer :: String -> Int -> LoggingT IO ()
ethAnalyzer server port = do
  $logInfo "Starting ethereum-analyzer."
  ver <- web3ClientVersion server port
  $logInfo $ T.append "version is " ver
  textCode <-
    ethGetCode server port "0xde0B295669a9FD93d5F28D9Ec85E40f4cb697BAe"
  $logInfo $ T.append "textCode is " textCode
  code <- getCode server port "0xde0B295669a9FD93d5F28D9Ec85E40f4cb697BAe"
  $logInfo $ T.pack $ "decompiled code is " ++ formatCode code
  return ()
