module Ethereum.Analyzer.Solidity.SimpleSpec
  ( spec
  ) where

import Protolude hiding (show)

import Data.Aeson
import Ethereum.Analyzer.Common
import Ethereum.Analyzer.Solidity
import Ethereum.Analyzer.TestData.DaoJson (simpleDaoJson)
import Ethereum.Analyzer.TestData.StorageJson (storageJson)
import GHC.Show (Show(..))
import Test.Hspec
import Text.PrettyPrint.Leijen.Text (Doc, pretty, renderPretty)

spec :: Spec
spec =
  describe "e2h" $ do
    it "parses storageJson" $ do
      let eitherContracts = (s2sContracts <$> eitherDecode (toS storageJson))
      eitherContracts `shouldBe` Left "something"
