-- | Launch ethereum-analyzer server.
module Main
    ( main
    ) where

import Blockchain.Analyzer.Servant (startApp)

main :: IO ()
main = startApp
