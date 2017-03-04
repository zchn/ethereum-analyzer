{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts
  #-}

-- | Dump all contracts.
module Main
  ( main
  ) where

import Blockchain.Jsonrpc.Client
import Blockchain.Output
import Blockchain.VM.Code
import Control.Monad.Logger
import Control.Monad.Logger
import Data.Text as T
import HFlags

defineFlag
  "jrpcServer"
  ("127.0.0.1" :: String)
  "Ethereum json-rpc server address."

defineFlag "jrpcPort" (8545 :: Int) "Ethereum json-rpc server port."

-- https://github.com/nilcons/hflags/issues/14
return []

main :: IO ()
main = do
  s <- $initHFlags "ea-dump-contract"
  putStrLn $ "Flags: " ++ show s
  flip runLoggingT printLogMsg (dumpContracts flags_jrpcServer flags_jrpcPort)

dumpContracts :: String -> Int -> LoggingT IO ()
dumpContracts server port = do
  $logInfo "Dumping eth contracts."
  ver <- web3ClientVersion server port
  $logInfo $ T.append "version is " ver
  bNum <- ethBlockNumber server port
  $logInfo $ T.append "block number is " bNum
  textCode <-
    ethGetCode server port "0xde0B295669a9FD93d5F28D9Ec85E40f4cb697BAe"
  $logInfo $ T.append "textCode is " textCode
  -- code <- getCode server port "0xde0B295669a9FD93d5F28D9Ec85E40f4cb697BAe"
  -- $logInfo $ T.pack $ "decompiled code is " ++ formatCode code
  return ()
