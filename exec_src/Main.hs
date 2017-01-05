{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts
  #-}

import Blockchain.Output
import Control.Monad.Logger
import Executable.EthAnalyzer

-- import HFlags
-- defineFlag "jrpcServer" ("127.0.0.1" :: String)
--   "Ethereum json-rpc server address."
-- defineFlag "jrpcPort" (8545 :: Int) "Ethereum json-rpc server port."
flags_jrpcServer = "192.168.3.53"

flags_jrpcPort = 7891 -- 8545

main :: IO ()
main
-- _ <- $initHFlags "Ethereum Analyzer"
 = do
  flip runLoggingT printLogMsg (ethAnalyzer flags_jrpcServer flags_jrpcPort)
