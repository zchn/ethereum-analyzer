module Ethereum.Analyzer.TestData.Asts
  ( solNodesOf
  , testFilepaths
  ) where

import Protolude

import Ethereum.Analyzer.Solidity

solNodesOf :: Text -> IO [SolNode]
solNodesOf path = do
  content <- readFile (toS path)
  case decodeSoleNodes $ toS content of
    Left err -> throwIO $ AssertionFailed $ toS err
    Right nodes -> return nodes

testFilepaths :: [Text]
testFilepaths =
  [ "../examples/co2.unica.it/ethereum/GaslessSend_42.ast.json"
  , "../examples/co2.unica.it/ethereum/SendTest.ast.json"
  , "../examples/co2.unica.it/ethereum/OddsAndEvens_0.4.2.ast.json"
  , "../examples/co2.unica.it/ethereum/SetProvider_0.4.2.ast.json"
  , "../examples/co2.unica.it/ethereum/SimpleDAO_0.4.2.ast.json"
  , "../examples/co2.unica.it/ethereum/KotET_0.4.2.ast.json"
  , "../examples/co2.unica.it/ethereum/Set_0.4.2.ast.json"
  , "../examples/co2.unica.it/ethereum/Bob_0.4.2.ast.json"
  , "../examples/slockit/DAO.ast.json"
  , "../examples/analysis-benchmark/selfdestruct-over-suicide.ast.json"
  , "../examples/solidity.readthedocs.io/solidity-by-example/voting.ast.json"
  , "../examples/solidity.readthedocs.io/control-structures/loops.ast.json"
  , "../examples/solidity.readthedocs.io/control-structures/tuple-min.ast.json"
  , "../examples/solidity.readthedocs.io/control-structures/tuple.ast.json"
  , "../examples/solidity.readthedocs.io/control-structures/tuple-min-0.4.17.ast.json"
  , "../examples/solidity.readthedocs.io/introduction-to-smart-contracts/storage.ast.json"
  , "../examples/solidity.readthedocs.io/introduction-to-smart-contracts/subcurrency.ast.json"
  ]
