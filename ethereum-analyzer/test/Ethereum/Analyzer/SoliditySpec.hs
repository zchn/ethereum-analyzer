module Ethereum.Analyzer.SoliditySpec
  ( spec
  ) where

import Protolude hiding (show)

import Data.Aeson
import Ethereum.Analyzer.Solidity
import Ethereum.Analyzer.TestData.DaoJson (simpleDaoJson)
import Ethereum.Analyzer.TestData.StorageJson (storageJson)
import Test.Hspec

spec :: Spec
spec =
  describe "e2h" $ do
    it "works for simpleDaoJson" $ do
      ((listToMaybe =<<) <$>) children <$>
        eitherDecode (toS simpleDaoJson) `shouldBe`
        Right
          (Just $
           defSolNode
           { attributes =
               Just $
               defSolNode {literals = Just ["solidity", "^", "0.4", ".2"]}
           , id = Just 1
           , name = Just "PragmaDirective"
           , src = Just "0:23:-1"
           })
    it "works for storageJson" $ do
      eitherDecode (toS storageJson) `shouldBe`
        Right
          (defSolNode
           { children =
               Just
                 [ defSolNode
                   { id = Just 1
                   , name = Just "PragmaDirective"
                   , src = Just "0:23:-1"
                   , attributes =
                       Just
                         (defSolNode
                          {literals = Just ["solidity", "^", "0.4", ".0"]})
                   , literals = Nothing
                   }
                 , defSolNode
                   { children =
                       Just
                         [ defSolNode
                           { children =
                               Just
                                 [ defSolNode
                                   { id = Just 2
                                   , name = Just "ElementaryTypeName"
                                   , src = Just "52:4:-1"
                                   , attributes =
                                       Just
                                         (defSolNode
                                          { name = Just "uint"
                                          , literals = Nothing
                                          })
                                   , literals = Nothing
                                   }
                                 ]
                           , id = Just 3
                           , name = Just "VariableDeclaration"
                           , src = Just "52:15:-1"
                           , attributes =
                               Just
                                 (defSolNode
                                  {name = Just "storedData", literals = Nothing})
                           , literals = Nothing
                           }
                         , defSolNode
                           { children =
                               Just
                                 [ defSolNode
                                   { children =
                                       Just
                                         [ defSolNode
                                           { children =
                                               Just
                                                 [ defSolNode
                                                   { id = Just 4
                                                   , name =
                                                       Just "ElementaryTypeName"
                                                   , src = Just "85:4:-1"
                                                   , attributes =
                                                       Just
                                                         (defSolNode
                                                          { name = Just "uint"
                                                          , literals = Nothing
                                                          })
                                                   , literals = Nothing
                                                   }
                                                 ]
                                           , id = Just 5
                                           , name = Just "VariableDeclaration"
                                           , src = Just "85:6:-1"
                                           , attributes =
                                               Just
                                                 (defSolNode
                                                  { name = Just "x"
                                                  , literals = Nothing
                                                  })
                                           , literals = Nothing
                                           }
                                         ]
                                   , id = Just 6
                                   , name = Just "ParameterList"
                                   , src = Just "84:8:-1"
                                   , literals = Nothing
                                   }
                                 , defSolNode
                                   { children = Just []
                                   , id = Just 7
                                   , name = Just "ParameterList"
                                   , src = Just "93:0:-1"
                                   , literals = Nothing
                                   }
                                 , defSolNode
                                   { children =
                                       Just
                                         [ defSolNode
                                           { children =
                                               Just
                                                 [ defSolNode
                                                   { children =
                                                       Just
                                                         [ defSolNode
                                                           { id = Just 8
                                                           , name =
                                                               Just "Identifier"
                                                           , src =
                                                               Just "99:10:-1"
                                                           , attributes =
                                                               Just
                                                                 (defSolNode
                                                                  { literals =
                                                                      Nothing
                                                                  })
                                                           , literals = Nothing
                                                           }
                                                         , defSolNode
                                                           { id = Just 9
                                                           , name =
                                                               Just "Identifier"
                                                           , src =
                                                               Just "112:1:-1"
                                                           , attributes =
                                                               Just
                                                                 (defSolNode
                                                                  { literals =
                                                                      Nothing
                                                                  })
                                                           , literals = Nothing
                                                           }
                                                         ]
                                                   , id = Just 10
                                                   , name = Just "Assignment"
                                                   , src = Just "99:14:-1"
                                                   , attributes =
                                                       Just
                                                         (defSolNode
                                                          {literals = Nothing})
                                                   , literals = Nothing
                                                   }
                                                 ]
                                           , id = Just 11
                                           , name = Just "ExpressionStatement"
                                           , src = Just "99:14:-1"
                                           , literals = Nothing
                                           }
                                         ]
                                   , id = Just 12
                                   , name = Just "Block"
                                   , src = Just "93:25:-1"
                                   , literals = Nothing
                                   }
                                 ]
                           , id = Just 13
                           , name = Just "FunctionDefinition"
                           , src = Just "72:46:-1"
                           , attributes =
                               Just
                                 (defSolNode
                                  {name = Just "set", literals = Nothing})
                           , literals = Nothing
                           }
                         , defSolNode
                           { children =
                               Just
                                 [ defSolNode
                                   { children = Just []
                                   , id = Just 14
                                   , name = Just "ParameterList"
                                   , src = Just "134:2:-1"
                                   , literals = Nothing
                                   }
                                 , defSolNode
                                   { children =
                                       Just
                                         [ defSolNode
                                           { children =
                                               Just
                                                 [ defSolNode
                                                   { id = Just 15
                                                   , name =
                                                       Just "ElementaryTypeName"
                                                   , src = Just "155:4:-1"
                                                   , attributes =
                                                       Just
                                                         (defSolNode
                                                          { name = Just "uint"
                                                          , literals = Nothing
                                                          })
                                                   , literals = Nothing
                                                   }
                                                 ]
                                           , id = Just 16
                                           , name = Just "VariableDeclaration"
                                           , src = Just "155:4:-1"
                                           , attributes =
                                               Just
                                                 (defSolNode
                                                  { name = Just ""
                                                  , literals = Nothing
                                                  })
                                           , literals = Nothing
                                           }
                                         ]
                                   , id = Just 17
                                   , name = Just "ParameterList"
                                   , src = Just "154:6:-1"
                                   , literals = Nothing
                                   }
                                 , defSolNode
                                   { children =
                                       Just
                                         [ defSolNode
                                           { children =
                                               Just
                                                 [ defSolNode
                                                   { id = Just 18
                                                   , name = Just "Identifier"
                                                   , src = Just "174:10:-1"
                                                   , attributes =
                                                       Just
                                                         (defSolNode
                                                          {literals = Nothing})
                                                   , literals = Nothing
                                                   }
                                                 ]
                                           , id = Just 19
                                           , name = Just "Return"
                                           , src = Just "167:17:-1"
                                           , literals = Nothing
                                           }
                                         ]
                                   , id = Just 20
                                   , name = Just "Block"
                                   , src = Just "161:28:-1"
                                   , literals = Nothing
                                   }
                                 ]
                           , id = Just 21
                           , name = Just "FunctionDefinition"
                           , src = Just "122:67:-1"
                           , attributes =
                               Just
                                 (defSolNode
                                  {name = Just "get", literals = Nothing})
                           , literals = Nothing
                           }
                         ]
                   , id = Just 22
                   , name = Just "ContractDefinition"
                   , src = Just "25:166:-1"
                   , attributes =
                       Just
                         (defSolNode
                          {name = Just "SimpleStorage", literals = Nothing})
                   , literals = Nothing
                   }
                 ]
           , name = Just "SourceUnit"
           , literals = Nothing
           })
