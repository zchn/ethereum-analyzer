module Ethereum.Analyzer.Solidity.ForeachSpec
  ( spec
  ) where

import Protolude hiding (show)

import Ethereum.Analyzer.Solidity
import Ethereum.Analyzer.TestData.StorageJson (storageJson)
import Test.Hspec

spec :: Spec
spec = do
  describe "statementsOf" $
    it "works for storageJson" $ do
      case decodeContracts storageJson of
        Right contracts ->
          sum (map (length . statementsOf) contracts) `shouldBe` 2
        Left err -> expectationFailure $ toS err
  describe "expressionsOf" $
    it "works for storageJson" $ do
      case decodeContracts storageJson of
        Right contracts ->
          sum (map (length . expressionsOf) contracts) `shouldBe` 1
        Left err -> expectationFailure $ toS err
