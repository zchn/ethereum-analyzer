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
import qualified Text.PrettyPrint.GenericPretty as GP

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
              [VarDecl {vName = Idfr "storedData", vType = Unknown "uint256"}]
          , cFunctions =
              [ FunDefinition
                { fName = Idfr "set"
                , fParams =
                    [VarDecl {vName = Idfr "x", vType = Unknown "uint256"}]
                , fReturns = []
                , fBody =
                    [ StAssign
                        (JustId (Idfr "storedData"))
                        (ExpLval (JustId (Idfr "x")))
                    ]
                }
              , FunDefinition
                { fName = Idfr "get"
                , fParams = []
                , fReturns =
                    [VarDecl {vName = Idfr "", vType = Unknown "uint256"}]
                , fBody = [StReturn [JustId (Idfr "storedData")]]
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
