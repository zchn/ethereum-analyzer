module Ethereum.Analyzer.SoliditySpec
  ( spec
  ) where

import Protolude hiding (show)

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
    it "pretty-prints simpleDaoJson" $ do
      let prettySol =
            (show <$> renderPretty 1.0 80) . (pretty :: [SolNode] -> Doc) <$>
            decodeSoleNodes (toS simpleDaoJson)
      -- putStrLn $ fromRight "" prettySol
      prettySol `shouldBe`
        Right
          ("[//--SourceUnit--\n" <>
           "contract SimpleDAO {mapping(address => uint256) credit\n" <>
           "                   ;fun donate (address to)(){(credit[to]+=msg.value)}\n" <>
           "                   ;fun withdraw (uint256 amount)\n" <>
           "                        ()\n" <>
           "                        {if((credit[msg.sender]>=amount)\n" <>
           "                           ,{bool res = msg.sender.call.value(amount)()\n" <>
           "                            ;(credit[msg.sender]-=amount)})}\n" <>
           "                   ;fun queryCredit (address to)(uint256){return(credit[to])}}\n" <>
           "contract Mallory {contract SimpleDAO dao\n" <>
           "                 ;address owner\n" <>
           "                 ;fun Mallory (contract SimpleDAO addr)\n" <>
           "                      ()\n" <>
           "                      {(owner=msg.sender);(dao=addr)}\n" <>
           "                 ;fun getJackpot ()(){bool res = owner.send(this.balance)}\n" <>
           "                 ;fun ()(){(dao.withdraw(dao.queryCredit(this)))}}\n" <>
           "contract Mallory2 {contract SimpleDAO dao\n" <>
           "                  ;address owner\n" <>
           "                  ;bool performAttack\n" <>
           "                  ;fun Mallory2 (contract SimpleDAO addr)\n" <>
           "                       ()\n" <>
           "                       {(owner=msg.sender);(dao=addr)}\n" <>
           "                  ;fun attack ()\n" <>
           "                       ()\n" <>
           "                       {(dao.donate.value(1)(this));(dao.withdraw(1))}\n" <>
           "                  ;fun getJackpot ()\n" <>
           "                       ()\n" <>
           "                       {(dao.withdraw(dao.balance))\n" <>
           "                       ;bool res = owner.send(this.balance)\n" <>
           "                       ;(performAttack=true)}\n" <>
           "                  ;fun ()\n" <>
           "                       ()\n" <>
           "                       {if(performAttack\n" <>
           "                          ,{(performAttack=false);(dao.withdraw(1))})}}]")
    it "pretty-prints storageJson" $ do
      let prettySol =
            (show <$> renderPretty 1.0 80) . (pretty :: [SolNode] -> Doc) <$>
            decodeSoleNodes (toS storageJson)
      -- putStrLn $ fromRight "" prettySol
      prettySol `shouldBe`
        Right
          ("[//--SourceUnit--\n" <>
           "contract SimpleStorage {uint256 storedData\n" <>
           "                       ;fun set (uint256 x)(){(storedData=x)}\n" <>
           "                       ;fun get ()(uint256){return(storedData)}}]")
    it "works for storageJson" $
      decodeSoleNodes (toS storageJson) `shouldBe`
      Right
        [ defSolNode
          { _AST =
              Just
                (defSolNode
                 { children =
                     Just
                       [ defSolNode
                         { _id = Just 1
                         , name = Just "PragmaDirective"
                         , src = Just "0:23:0"
                         , attributes =
                             Just
                               (defSolNode
                                {literals = Just ["solidity", "^", "0.4", ".0"]})
                         }
                       , defSolNode
                         { children =
                             Just
                               [ defSolNode
                                 { children =
                                     Just
                                       [ defSolNode
                                         { _id = Just 2
                                         , name = Just "ElementaryTypeName"
                                         , src = Just "52:4:0"
                                         , attributes =
                                             Just
                                               (defSolNode {name = Just "uint"})
                                         }
                                       ]
                                 , _id = Just 3
                                 , name = Just "VariableDeclaration"
                                 , src = Just "52:15:0"
                                 , attributes =
                                     Just
                                       (defSolNode
                                        { name = Just "storedData"
                                        , _type = Just "uint256"
                                        , visibility = Just "internal"
                                        , constant = Just False
                                        , storageLocation = Just "default"
                                        })
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
                                                         { _id = Just 4
                                                         , name =
                                                             Just
                                                               "ElementaryTypeName"
                                                         , src = Just "85:4:0"
                                                         , attributes =
                                                             Just
                                                               (defSolNode
                                                                { name =
                                                                    Just "uint"
                                                                })
                                                         }
                                                       ]
                                                 , _id = Just 5
                                                 , name =
                                                     Just "VariableDeclaration"
                                                 , src = Just "85:6:0"
                                                 , attributes =
                                                     Just
                                                       (defSolNode
                                                        { name = Just "x"
                                                        , _type = Just "uint256"
                                                        , visibility =
                                                            Just "internal"
                                                        , constant = Just False
                                                        , storageLocation =
                                                            Just "default"
                                                        })
                                                 }
                                               ]
                                         , _id = Just 6
                                         , name = Just "ParameterList"
                                         , src = Just "84:8:0"
                                         }
                                       , defSolNode
                                         { children = Just []
                                         , _id = Just 7
                                         , name = Just "ParameterList"
                                         , src = Just "93:0:0"
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
                                                                 { _id = Just 8
                                                                 , name =
                                                                     Just
                                                                       "Identifier"
                                                                 , src =
                                                                     Just
                                                                       "99:10:0"
                                                                 , attributes =
                                                                     Just
                                                                       (defSolNode
                                                                        { _type =
                                                                            Just
                                                                              "uint256"
                                                                        , value =
                                                                            Just
                                                                              "storedData"
                                                                        })
                                                                 }
                                                               , defSolNode
                                                                 { _id = Just 9
                                                                 , name =
                                                                     Just
                                                                       "Identifier"
                                                                 , src =
                                                                     Just
                                                                       "112:1:0"
                                                                 , attributes =
                                                                     Just
                                                                       (defSolNode
                                                                        { _type =
                                                                            Just
                                                                              "uint256"
                                                                        , value =
                                                                            Just
                                                                              "x"
                                                                        })
                                                                 }
                                                               ]
                                                         , _id = Just 10
                                                         , name =
                                                             Just "Assignment"
                                                         , src = Just "99:14:0"
                                                         , attributes =
                                                             Just
                                                               (defSolNode
                                                                { _type =
                                                                    Just
                                                                      "uint256"
                                                                , operator =
                                                                    Just "="
                                                                })
                                                         }
                                                       ]
                                                 , _id = Just 11
                                                 , name =
                                                     Just "ExpressionStatement"
                                                 , src = Just "99:14:0"
                                                 }
                                               ]
                                         , _id = Just 12
                                         , name = Just "Block"
                                         , src = Just "93:25:0"
                                         }
                                       ]
                                 , _id = Just 13
                                 , name = Just "FunctionDefinition"
                                 , src = Just "72:46:0"
                                 , attributes =
                                     Just
                                       (defSolNode
                                        { name = Just "set"
                                        , visibility = Just "public"
                                        , payable = Just False
                                        , constant = Just False
                                        })
                                 }
                               , defSolNode
                                 { children =
                                     Just
                                       [ defSolNode
                                         { children = Just []
                                         , _id = Just 14
                                         , name = Just "ParameterList"
                                         , src = Just "134:2:0"
                                         }
                                       , defSolNode
                                         { children =
                                             Just
                                               [ defSolNode
                                                 { children =
                                                     Just
                                                       [ defSolNode
                                                         { _id = Just 15
                                                         , name =
                                                             Just
                                                               "ElementaryTypeName"
                                                         , src = Just "155:4:0"
                                                         , attributes =
                                                             Just
                                                               (defSolNode
                                                                { name =
                                                                    Just "uint"
                                                                })
                                                         }
                                                       ]
                                                 , _id = Just 16
                                                 , name =
                                                     Just "VariableDeclaration"
                                                 , src = Just "155:4:0"
                                                 , attributes =
                                                     Just
                                                       (defSolNode
                                                        { name = Just ""
                                                        , _type = Just "uint256"
                                                        , visibility =
                                                            Just "internal"
                                                        , constant = Just False
                                                        , storageLocation =
                                                            Just "default"
                                                        })
                                                 }
                                               ]
                                         , _id = Just 17
                                         , name = Just "ParameterList"
                                         , src = Just "154:6:0"
                                         }
                                       , defSolNode
                                         { children =
                                             Just
                                               [ defSolNode
                                                 { children =
                                                     Just
                                                       [ defSolNode
                                                         { _id = Just 18
                                                         , name =
                                                             Just "Identifier"
                                                         , src =
                                                             Just "174:10:0"
                                                         , attributes =
                                                             Just
                                                               (defSolNode
                                                                { _type =
                                                                    Just
                                                                      "uint256"
                                                                , value =
                                                                    Just
                                                                      "storedData"
                                                                })
                                                         }
                                                       ]
                                                 , _id = Just 19
                                                 , name = Just "Return"
                                                 , src = Just "167:17:0"
                                                 }
                                               ]
                                         , _id = Just 20
                                         , name = Just "Block"
                                         , src = Just "161:28:0"
                                         }
                                       ]
                                 , _id = Just 21
                                 , name = Just "FunctionDefinition"
                                 , src = Just "122:67:0"
                                 , attributes =
                                     Just
                                       (defSolNode
                                        { name = Just "get"
                                        , visibility = Just "public"
                                        , payable = Just False
                                        , constant = Just True
                                        })
                                 }
                               ]
                         , _id = Just 22
                         , name = Just "ContractDefinition"
                         , src = Just "25:166:0"
                         , attributes =
                             Just
                               (defSolNode
                                { name = Just "SimpleStorage"
                                , fullyImplemented = Just True
                                , isLibrary = Just False
                                , linearizedBaseContracts = Just [22]
                                })
                         }
                       ]
                 , name = Just "SourceUnit"
                 })
          }
        ]
