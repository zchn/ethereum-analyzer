{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts
  #-}

-- | Dump all contracts.
module Main
  ( main
  ) where

import Ethereum.Jsonrpc.Client
import Blockchain.Output
import Control.Monad.Logger
import Control.Monad.Trans
import Data.Maybe
import Data.Text as T hiding (map)
import HFlags
import Numeric
import System.Directory as SD

defineFlag
  "jrpcServer"
  ("127.0.0.1" :: String)
  "Ethereum json-rpc server address."

defineFlag "jrpcPort" (8545 :: Int) "Ethereum json-rpc server port."

defineFlag "contractDir" ("contracts" :: String) "Directory for contact files."

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
  return ()

enumerateContractAt :: String -> Int -> Int -> LoggingT IO ()
enumerateContractAt _ _ 0 = return ()
enumerateContractAt s p bn = do
  $logInfo $ T.pack $ "processing block " ++ show bn
  transactions <- ethGetTransactionsByBlockNumber s p (T.pack $ show bn)
  -- $logInfo $ T.pack $ "transactions: " ++ show transactions
  addresses <- catMaybes <$> mapM (ethGetContractAddrByTxHash s p) transactions
  -- $logInfo $ T.pack $ "addresses: " ++ show addresses
  mapM_
    (\addr -> do
       let fpath = flags_contractDir ++ "/" ++ T.unpack addr ++ ".contract"
       fileExists <- lift $ doesFileExist fpath
       if fileExists
         then $logInfo $ T.pack $ "skipping: " ++ show addr
         else do
           textCode <- ethGetCode s p addr
           lift $ writeFile fpath $ T.unpack $ T.drop 2 textCode
     -- $logInfo $ T.append "textCode is " textCode
     )
    addresses
  -- code <- getCode server port "0xde0B295669a9FD93d5F28D9Ec85E40f4cb697BAe"
  -- $logInfo $ T.pack $ "decompiled code is " ++ formatCode code
  enumerateContractAt s p (bn - 1)
