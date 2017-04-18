-- | Launch ethereum-analyzer server.
module Main
  ( main
  ) where

import Ethereum.Analyzer.Servant (startApp)

main :: IO ()
main = startApp
