{-# LANGUAGE OverloadedStrings #-}

module Ethereum.Analyzer.Solidity.AstJsonSpec
  ( spec
  ) where

import Protolude

import Ethereum.Analyzer.Solidity
import Ethereum.Analyzer.TestData.Asts
import Test.Hspec

spec :: Spec
spec =
  describe "AstJson" $ mapM_ nonEmptyAstFile
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
    , "../examples/solidity.readthedocs.io/introduction-to-smart-contracts/subcurrency.ast.json" ]

nonEmptyAstFile :: Text -> SpecWith ()
nonEmptyAstFile filepath = do
  context (toS filepath) $
    it "returns [SolNode]" $ do
      nodes <- solNodesOf filepath
      length (nodes :: [SolNode]) `shouldSatisfy` (0 <)
