module Ethereum.Analyzer.SoliditySpec
  ( spec
  ) where

import Protolude hiding (show)

import Data.Aeson
import Data.Default
import Ethereum.Analyzer.Solidity
import Ethereum.Analyzer.TestData.DaoJson (simpleDaoJson)
import Test.Hspec

spec :: Spec
spec =
  describe "e2h" $ do
    it "works for hexstring1" $ do
      eitherDecode (toS simpleDaoJson) `shouldBe` Right (
        SolNode {
            attributes = Just $ SolNode {
                attributes = Just $ SolNode {
                    literals = Just [""],
                    id = Just 1,
                    name = Just "name",
                    src = Just "src"
                    }
                }
            }
        )
