module Ethereum.Analyzer.SoliditySpec
  ( spec
  ) where

import Protolude hiding (show)

import Data.Aeson
import Ethereum.Analyzer.Solidity
import Ethereum.Analyzer.TestData.DaoJson (simpleDaoJson)
import Test.Hspec

spec :: Spec
spec =
  describe "e2h" $ do
    it "works for hexstring1" $ do
      ((listToMaybe =<<) <$>) children <$> eitherDecode (toS simpleDaoJson)
        `shouldBe` Right (Just $
                          defSolNode
                          { attributes = Just $
                                         defSolNode
                                         { literals = Just
                                           [ "solidity"
                                           , "^"
                                           , "0.4"
                                           , ".2"
                                           ]}
                          , id = Just 1
                          , name = Just "PragmaDirective"
                          , src = Just "0:23:-1"
                          })
