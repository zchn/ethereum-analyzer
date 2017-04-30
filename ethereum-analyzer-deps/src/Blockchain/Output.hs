module Blockchain.Output where

import Control.Monad.Logger
import qualified Data.ByteString.Char8 as BC
import System.GlobalLock
import System.Log.FastLogger

printLogMsg :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
--printLogMsg loc logSource level msg = do
printLogMsg _ _ _ msg = lock $ putStrLn $ BC.unpack $ fromLogStr msg

printToFile :: FilePath -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
--printLogMsg loc logSource level msg = do
printToFile path _ _ _ msg =
  lock $ appendFile path $ BC.unpack (fromLogStr msg) ++ "\n"
