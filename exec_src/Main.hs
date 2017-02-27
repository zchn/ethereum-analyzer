-- | Launch ethereum-analyzer server.
module Main
    ( main
    ) where

import Blockchain.Analyze.Servant (startApp)

main :: IO ()
main = startApp
