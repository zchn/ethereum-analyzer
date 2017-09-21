{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Dump all contracts.
module Main
  ( main
  ) where

import Protolude

import Blockchain.Output
import Control.Monad.Logger
import Ethereum.Executable.DumpCodeMain
import HFlags

defineFlag
  "jrpcServer"
  ("127.0.0.1" :: Text)
  "Ethereum json-rpc server address."

defineFlag "jrpcPort" (8545 :: Int) "Ethereum json-rpc server port."

defineFlag "contractDir" ("contracts" :: Text) "Directory for contact files."

defineFlag
  "dummy"
  False
  "dummy flag for https://github.com/nilcons/hflags/issues/14"

main :: IO ()
main = do
  s <- $initHFlags "ea-dump-contract"
  putStrLn $ "Flags: " ++ show s
  runLoggingT
    (dumpContracts flags_jrpcServer flags_jrpcPort flags_contractDir)
    printLogMsg
