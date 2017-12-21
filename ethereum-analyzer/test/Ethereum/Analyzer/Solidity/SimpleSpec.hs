module Ethereum.Analyzer.Solidity.SimpleSpec
  ( spec
  ) where

import Protolude hiding (show)

import Compiler.Hoopl

-- import Ethereum.Analyzer.Common
import Ethereum.Analyzer.Solidity
import Ethereum.Analyzer.TestData.Asts
import Ethereum.Analyzer.TestData.StorageJson (storageJson)

-- import GHC.Show (Show(..))
import Test.Hspec

spec :: Spec
spec = do
  describe "e2h" $
    it "parses storageJson" $ do
      let eitherContracts = do
            solNodes <- decodeSoleNodes (toS storageJson)
            let mContracts = mapM s2sContracts solNodes
            let contracts = concat $ runSimpleUniqueMonad mContracts
            return contracts
    -- GP.pp eitherContracts
      eitherContracts `shouldBe`
        Right
          [ Contract
            { cName = "SimpleStorage"
            , cStateVars =
                [ VarDecl
                  {vName = Idfr {unIdfr = "storedData"}, vType = Uint256}
                ]
            , cFunctions =
                [ FunDefinition
                  { fName = Idfr {unIdfr = "set"}
                  , fParams =
                      [VarDecl {vName = Idfr {unIdfr = "x"}, vType = Uint256}]
                  , fReturns = []
                  , fBody =
                      [ StAssign
                          (JustId (Idfr {unIdfr = "storedData"}))
                          (ExpLval (JustId (Idfr {unIdfr = "x"})))
                      ]
                  }
                , FunDefinition
                  { fName = Idfr {unIdfr = "get"}
                  , fParams = []
                  , fReturns =
                      [VarDecl {vName = Idfr {unIdfr = ""}, vType = Uint256}]
                  , fBody = [StReturn [JustId (Idfr {unIdfr = "storedData"})]]
                  }
                ]
            }
          ]
  describe "Simple" $ mapM_ nonEmptySimple testFilepaths

nonEmptySimple :: Text -> SpecWith ()
nonEmptySimple filepath =
  context (toS filepath) $
  it "converts SolNode to Contracts" $ do
    contracts <-
      do solNodes <- solNodesOf filepath
         let mContracts = mapM s2sContracts solNodes
         let contracts = concat $ runSimpleUniqueMonad mContracts
         return contracts
    length contracts `shouldSatisfy` (0 <)
