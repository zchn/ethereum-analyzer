-- | Launch ethereum-analyzer server.
module Main
  ( main
  ) where

import Ethereum.Analyzer.Web (startApp)

main :: IO ()
main = startApp
