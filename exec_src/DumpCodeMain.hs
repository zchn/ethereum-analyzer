{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts
  #-}

-- | Dump all contracts.
module Main
  ( main
  ) where

import Blockchain.Jsonrpc.Client
import Blockchain.Output
import Blockchain.VM.Code
import Control.Monad
import Control.Monad.Logger
import Data.Text as T
import HFlags
import Numeric

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
  latest_bNum <- ethBlockNumber server port
  let parsed_bns = readHex $ Prelude.drop 2 $ T.unpack latest_bNum
  $logInfo $ T.pack $ "parsing block number: " ++ show parsed_bns
  let latest_bn = fst $ Prelude.head $ parsed_bns
  $logInfo $ T.pack $ "block number is " ++ show latest_bn
  enumerateContractAt server port latest_bn
  textCode <-
    ethGetCode server port "0xde0B295669a9FD93d5F28D9Ec85E40f4cb697BAe"
  $logInfo $ T.append "textCode is " textCode
  -- code <- getCode server port "0xde0B295669a9FD93d5F28D9Ec85E40f4cb697BAe"
  -- $logInfo $ T.pack $ "decompiled code is " ++ formatCode code
  return ()

enumerateContractAt :: String -> Int -> Int -> LoggingT IO ()
enumerateContractAt _ _ 0 = return ()
enumerateContractAt s p bn = do
  transactions <- ethGetTransactionsByBlockNumber s p (T.pack $ show bn)
  $logInfo $ T.pack $ "transactions: " ++ show transactions
  enumerateContractAt s p (bn - 1)
