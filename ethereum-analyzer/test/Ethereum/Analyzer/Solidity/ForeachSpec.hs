module Ethereum.Analyzer.Solidity.ForeachSpec
  ( spec
  ) where

import Protolude hiding (show)

import Ethereum.Analyzer.Solidity
import Ethereum.Analyzer.TestData.DaoJson (simpleDaoJson)
import Ethereum.Analyzer.TestData.StorageJson (storageJson)
import Test.Hspec

spec :: Spec
spec = do
  describe "statementsOf" $
    parallel $ do
      it "works for storageJson" $ do
        case decodeContracts storageJson of
          Right contracts ->
            sum (map (length . statementsOf) contracts) `shouldBe` 2
          Left err -> expectationFailure $ toS err
      it "works for simpleDaoJson" $ do
        case decodeContracts simpleDaoJson of
          Right contracts ->
            sum (map (length . statementsOf) contracts) `shouldBe` 39
          Left err -> expectationFailure $ toS err
  describe "expressionsOf" $
    parallel $ do
      it "works for storageJson" $ do
        case decodeContracts storageJson of
          Right contracts ->
            sum (map (length . expressionsOf) contracts) `shouldBe` 1
          Left err -> expectationFailure $ toS err
      it "works for simpleDaoJson" $ do
        case decodeContracts simpleDaoJson of
          Right contracts ->
            sum (map (length . expressionsOf) contracts) `shouldBe` 33
          Left err -> expectationFailure $ toS err
