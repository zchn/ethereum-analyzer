module Ethereum.Analyzer.TestData.Asts
    ( solNodesOf
    ) where

import Protolude

import           Ethereum.Analyzer.Solidity

solNodesOf :: Text -> IO [SolNode]
solNodesOf path = do
  content <- readFile (toS path)
  case decodeSoleNodes $ toS content of
    Left err -> throwIO $ AssertionFailed $ toS err
    Right nodes -> return nodes
