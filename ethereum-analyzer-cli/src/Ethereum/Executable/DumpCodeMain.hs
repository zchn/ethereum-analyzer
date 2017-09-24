{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts
  #-}

-- | Dump all contracts.
module Ethereum.Executable.DumpCodeMain
  ( dumpContracts
  ) where

import Prelude

import Control.Monad.Logger
import Control.Monad.Trans
import Data.Maybe
import Data.Text as T hiding (map)
import Ethereum.Jsonrpc.Client
import Numeric
import System.Directory as SD

dumpContracts :: String -> Int -> String -> LoggingT IO ()
dumpContracts server port contractDir = do
  $logInfo "Dumping eth contracts."
  ver <- web3ClientVersion server port
  $logInfo $ T.append "version is " ver
  latest_bNum <- ethBlockNumber server port
  let parsed_bns = readHex $ Prelude.drop 2 $ T.unpack latest_bNum
  $logInfo $ T.pack $ "parsing block number: " ++ show parsed_bns
  let latest_bn = fst $ Prelude.head parsed_bns
  $logInfo $ T.pack $ "block number is " ++ show latest_bn
  enumerateContractAt server port contractDir latest_bn
  return ()

enumerateContractAt :: String -> Int -> String -> Int -> LoggingT IO ()
enumerateContractAt _ _ _ 0 = return ()
enumerateContractAt s p contractDir bn = do
  $logInfo $ T.pack $ "processing block " ++ show bn
  transactions <- ethGetTransactionsByBlockNumber s p (T.pack $ show bn)
  addresses <- catMaybes <$> mapM (ethGetContractAddrByTxHash s p) transactions
  mapM_
    (\addr -> do
       let fpath = contractDir ++ "/" ++ T.unpack addr ++ ".contract"
       fileExists <- lift $ doesFileExist fpath
       if fileExists
         then $logInfo $ T.pack $ "skipping: " ++ show addr
         else do
           textCode <- ethGetCode s p addr
           lift $ writeFile fpath $ T.unpack $ T.drop 2 textCode)
    addresses
  enumerateContractAt s p contractDir (bn - 1)
