{-# LANGUAGE OverloadedStrings #-}

module Ethereum.Analyzer.Solidity.AstJsonSpec
  ( spec
  ) where

import Protolude

import Ethereum.Analyzer.Solidity
import Ethereum.Analyzer.TestData.Asts
import Test.Hspec

spec :: Spec
spec = describe "AstJson" $ mapM_ nonEmptyAstFile testFilepaths

nonEmptyAstFile :: Text -> SpecWith ()
nonEmptyAstFile filepath =
  context (toS filepath) $
  it "returns [SolNode]" $ do
    nodes <- solNodesOf filepath
    length (nodes :: [SolNode]) `shouldSatisfy` (0 <)
