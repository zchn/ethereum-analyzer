module Ethereum.Analyzer.Solidity.ForeachSpec
  ( spec
  ) where

import Protolude hiding (show)

import Compiler.Hoopl

-- import Ethereum.Analyzer.Common
import Ethereum.Analyzer.Solidity
import Ethereum.Analyzer.TestData.DaoJson (simpleDaoJson)
import Ethereum.Analyzer.TestData.StorageJson (storageJson)

-- import GHC.Show (Show(..))
import Test.Hspec
import qualified Text.PrettyPrint.GenericPretty as GP

-- TODO(zchn): Mark all specs in ethereum-analyzer parallel.

spec :: Spec
spec = do
  describe "statementsOf" $ do
    it "works for storageJson" $ do
      case decodeContracts storageJson of
        Right contracts -> sum (map (length . statementsOf) contracts) `shouldBe` 2
        Left err -> expectationFailure $ toS err
    it "works for simpleDaoJson" $ do
      case decodeContracts simpleDaoJson of
        Right contracts -> sum (map (length . statementsOf) contracts) `shouldBe` 39
        Left err -> expectationFailure $ toS err
  describe "expressionsOf" $ do
    it "works for storageJson" $ do
      case decodeContracts storageJson of
        Right contracts -> sum (map (length . expressionsOf) contracts) `shouldBe` 1
        Left err -> expectationFailure $ toS err
    it "works for simpleDaoJson" $ do
      case decodeContracts simpleDaoJson of
        Right contracts -> sum (map (length . expressionsOf) contracts) `shouldBe` 33
        Left err -> expectationFailure $ toS err
