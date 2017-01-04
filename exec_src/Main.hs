{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}

import Blockchain.Output
import Control.Monad.Logger
import Executable.EthAnalyzer
-- import HFlags

-- defineFlag "jrpcServer" ("127.0.0.1" :: String)
--   "Ethereum json-rpc server address."
-- defineFlag "jrpcPort" (8545 :: Int) "Ethereum json-rpc server port."

flags_jrpcServer = "127.0.0.1"
flags_jrpcPort = 8545

main :: IO ()
main = do
  _ <- $initHFlags "Ethereum Analyzer"
  flip runLoggingT printLogMsg (ethAnalyzer flags_jrpcServer flags_jrpcPort)
