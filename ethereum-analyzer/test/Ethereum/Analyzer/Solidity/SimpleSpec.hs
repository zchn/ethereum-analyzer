module Ethereum.Analyzer.Solidity.SimpleSpec
  ( spec
  ) where

import Protolude hiding (show)

import Data.Aeson

-- import Ethereum.Analyzer.Common
import Ethereum.Analyzer.Solidity
import Ethereum.Analyzer.TestData.DaoJson (simpleDaoJson)
import Ethereum.Analyzer.TestData.StorageJson (storageJson)

-- import GHC.Show (Show(..))
import Test.Hspec
import qualified Text.PrettyPrint.GenericPretty as GP

spec :: Spec
spec =
  describe "e2h" $ do
    it "parses storageJson" $ do
      let eitherContracts = (s2sContracts <$> eitherDecode (toS storageJson))
      GP.pp eitherContracts
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
    it "parses simpleDaoJson" $ do
      let eitherContracts = (s2sContracts <$> eitherDecode (toS simpleDaoJson))
      GP.pp eitherContracts
      eitherContracts `shouldBe`
        Right
          [ Contract
            { cName = "SimpleDAO"
            , cStateVars =
                [ VarDecl
                  { vName = Idfr "credit"
                  , vType = Unknown "mapping(address => uint256)"
                  }
                ]
            , cFunctions =
                [ FunDefinition
                  { fName = Idfr "donate"
                  , fParams =
                      [VarDecl {vName = Idfr "to", vType = Unknown "address"}]
                  , fReturns = []
                  , fBody =
                      [ StAssign
                          (Index
                           { iArray = JustId (Idfr "credit")
                           , iIndex =
                               Index
                               { iArray = JustId (Idfr "credit")
                               , iIndex = JustId (Idfr "to")
                               }
                           })
                          (ExpLval
                             (Member
                              { mObj = JustId (Idfr "msg")
                              , mField = Idfr "value"
                              }))
                      ]
                  }
                , FunDefinition
                  { fName = Idfr "withdraw"
                  , fParams =
                      [ VarDecl
                        {vName = Idfr "amount", vType = Unknown "uint256"}
                      ]
                  , fReturns = []
                  , fBody =
                      [ StAssign
                          (JustId (Idfr "TodoTmp"))
                          (ExpBin
                             ">="
                             (Index
                              { iArray = JustId (Idfr "credit")
                              , iIndex =
                                  Index
                                  { iArray = JustId (Idfr "credit")
                                  , iIndex =
                                      Member
                                      { mObj = JustId (Idfr "msg")
                                      , mField = Idfr "sender"
                                      }
                                  }
                              })
                             (JustId (Idfr "amount")))
                      , StIf
                          (JustId (Idfr "TodoTmp"))
                          [ StAssign
                              (JustId (Idfr "TodoTmp"))
                              (ExpCall
                                 (Member
                                  { mObj =
                                      Member
                                      { mObj =
                                          Member
                                          { mObj = JustId (Idfr "msg")
                                          , mField = Idfr "sender"
                                          }
                                      , mField = Idfr "call"
                                      }
                                  , mField = Idfr "value"
                                  })
                                 [JustId (Idfr "amount")])
                          , StAssign
                              (JustId (Idfr "TodoTmp"))
                              (ExpCall (JustId (Idfr "TodoTmp")) [])
                          , StLocalVarDecl
                              (VarDecl
                               {vName = Idfr "res", vType = Unknown "bool"})
                          , StAssign
                              (JustId (Idfr "res"))
                              (ExpLval (JustId (Idfr "TodoTmp")))
                          , StAssign
                              (Index
                               { iArray = JustId (Idfr "credit")
                               , iIndex =
                                   Index
                                   { iArray = JustId (Idfr "credit")
                                   , iIndex =
                                       Member
                                       { mObj = JustId (Idfr "msg")
                                       , mField = Idfr "sender"
                                       }
                                   }
                               })
                              (ExpLval (JustId (Idfr "amount")))
                          ]
                          []
                      ]
                  }
                , FunDefinition
                  { fName = Idfr "queryCredit"
                  , fParams =
                      [VarDecl {vName = Idfr "to", vType = Unknown "address"}]
                  , fReturns =
                      [VarDecl {vName = Idfr "", vType = Unknown "uint256"}]
                  , fBody =
                      [ StReturn
                          [ Index
                            { iArray = JustId (Idfr "credit")
                            , iIndex =
                                Index
                                { iArray = JustId (Idfr "credit")
                                , iIndex = JustId (Idfr "to")
                                }
                            }
                          ]
                      ]
                  }
                ]
            }
          , Contract
            { cName = "Mallory"
            , cStateVars =
                [ VarDecl
                  {vName = Idfr "dao", vType = Unknown "contract SimpleDAO"}
                , VarDecl {vName = Idfr "owner", vType = Unknown "address"}
                ]
            , cFunctions =
                [ FunDefinition
                  { fName = Idfr "Mallory"
                  , fParams =
                      [ VarDecl
                        { vName = Idfr "addr"
                        , vType = Unknown "contract SimpleDAO"
                        }
                      ]
                  , fReturns = []
                  , fBody =
                      [ StAssign
                          (JustId (Idfr "owner"))
                          (ExpLval
                             (Member
                              { mObj = JustId (Idfr "msg")
                              , mField = Idfr "sender"
                              }))
                      , StAssign
                          (JustId (Idfr "dao"))
                          (ExpLval (JustId (Idfr "addr")))
                      ]
                  }
                , FunDefinition
                  { fName = Idfr "getJackpot"
                  , fParams = []
                  , fReturns = []
                  , fBody =
                      [ StAssign
                          (JustId (Idfr "TodoTmp"))
                          (ExpCall
                             (Member
                              { mObj = JustId (Idfr "owner")
                              , mField = Idfr "send"
                              })
                             [ Member
                               { mObj = JustId (Idfr "this")
                               , mField = Idfr "balance"
                               }
                             ])
                      , StLocalVarDecl
                          (VarDecl {vName = Idfr "res", vType = Unknown "bool"})
                      , StAssign
                          (JustId (Idfr "res"))
                          (ExpLval (JustId (Idfr "TodoTmp")))
                      ]
                  }
                , FunDefinition
                  { fName = Idfr ""
                  , fParams = []
                  , fReturns = []
                  , fBody =
                      [ StAssign
                          (JustId (Idfr "TodoTmp"))
                          (ExpCall
                             (Member
                              { mObj = JustId (Idfr "dao")
                              , mField = Idfr "queryCredit"
                              })
                             [JustId (Idfr "this")])
                      , StAssign
                          (JustId (Idfr "TodoTmp"))
                          (ExpCall
                             (Member
                              { mObj = JustId (Idfr "dao")
                              , mField = Idfr "withdraw"
                              })
                             [JustId (Idfr "TodoTmp")])
                      , StAssign
                          (JustId (Idfr "_"))
                          (ExpLval (JustId (Idfr "TodoTmp")))
                      ]
                  }
                ]
            }
          , Contract
            { cName = "Mallory2"
            , cStateVars =
                [ VarDecl
                  {vName = Idfr "dao", vType = Unknown "contract SimpleDAO"}
                , VarDecl {vName = Idfr "owner", vType = Unknown "address"}
                , VarDecl {vName = Idfr "performAttack", vType = Unknown "bool"}
                ]
            , cFunctions =
                [ FunDefinition
                  { fName = Idfr "Mallory2"
                  , fParams =
                      [ VarDecl
                        { vName = Idfr "addr"
                        , vType = Unknown "contract SimpleDAO"
                        }
                      ]
                  , fReturns = []
                  , fBody =
                      [ StAssign
                          (JustId (Idfr "owner"))
                          (ExpLval
                             (Member
                              { mObj = JustId (Idfr "msg")
                              , mField = Idfr "sender"
                              }))
                      , StAssign
                          (JustId (Idfr "dao"))
                          (ExpLval (JustId (Idfr "addr")))
                      ]
                  }
                , FunDefinition
                  { fName = Idfr "attack"
                  , fParams = []
                  , fReturns = []
                  , fBody =
                      [ StAssign (JustId (Idfr "TodotTmp")) (ExpLiteral "1")
                      , StAssign
                          (JustId (Idfr "TodoTmp"))
                          (ExpCall
                             (Member
                              { mObj =
                                  Member
                                  { mObj = JustId (Idfr "dao")
                                  , mField = Idfr "donate"
                                  }
                              , mField = Idfr "value"
                              })
                             [JustId (Idfr "TodoTmp")])
                      , StAssign
                          (JustId (Idfr "TodoTmp"))
                          (ExpCall
                             (JustId (Idfr "TodoTmp"))
                             [JustId (Idfr "this")])
                      , StAssign
                          (JustId (Idfr "_"))
                          (ExpLval (JustId (Idfr "TodoTmp")))
                      , StAssign (JustId (Idfr "TodotTmp")) (ExpLiteral "1")
                      , StAssign
                          (JustId (Idfr "TodoTmp"))
                          (ExpCall
                             (Member
                              { mObj = JustId (Idfr "dao")
                              , mField = Idfr "withdraw"
                              })
                             [JustId (Idfr "TodoTmp")])
                      , StAssign
                          (JustId (Idfr "_"))
                          (ExpLval (JustId (Idfr "TodoTmp")))
                      ]
                  }
                , FunDefinition
                  { fName = Idfr "getJackpot"
                  , fParams = []
                  , fReturns = []
                  , fBody =
                      [ StAssign
                          (JustId (Idfr "TodoTmp"))
                          (ExpCall
                             (Member
                              { mObj = JustId (Idfr "dao")
                              , mField = Idfr "withdraw"
                              })
                             [ Member
                               { mObj = JustId (Idfr "dao")
                               , mField = Idfr "balance"
                               }
                             ])
                      , StAssign
                          (JustId (Idfr "_"))
                          (ExpLval (JustId (Idfr "TodoTmp")))
                      , StAssign
                          (JustId (Idfr "TodoTmp"))
                          (ExpCall
                             (Member
                              { mObj = JustId (Idfr "owner")
                              , mField = Idfr "send"
                              })
                             [ Member
                               { mObj = JustId (Idfr "this")
                               , mField = Idfr "balance"
                               }
                             ])
                      , StLocalVarDecl
                          (VarDecl {vName = Idfr "res", vType = Unknown "bool"})
                      , StAssign
                          (JustId (Idfr "res"))
                          (ExpLval (JustId (Idfr "TodoTmp")))
                      , StAssign (JustId (Idfr "TodotTmp")) (ExpLiteral "true")
                      , StAssign
                          (JustId (Idfr "performAttack"))
                          (ExpLval (JustId (Idfr "TodoTmp")))
                      ]
                  }
                , FunDefinition
                  { fName = Idfr ""
                  , fParams = []
                  , fReturns = []
                  , fBody =
                      [ StIf
                          (JustId (Idfr "performAttack"))
                          [ StAssign
                              (JustId (Idfr "TodotTmp"))
                              (ExpLiteral "false")
                          , StAssign
                              (JustId (Idfr "performAttack"))
                              (ExpLval (JustId (Idfr "TodoTmp")))
                          , StAssign (JustId (Idfr "TodotTmp")) (ExpLiteral "1")
                          , StAssign
                              (JustId (Idfr "TodoTmp"))
                              (ExpCall
                                 (Member
                                  { mObj = JustId (Idfr "dao")
                                  , mField = Idfr "withdraw"
                                  })
                                 [JustId (Idfr "TodoTmp")])
                          , StAssign
                              (JustId (Idfr "_"))
                              (ExpLval (JustId (Idfr "TodoTmp")))
                          ]
                          []
                      ]
                  }
                ]
            }
          ]
