module Ethereum.Analyzer.Solidity.SimpleSpec
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

spec :: Spec
spec =
  describe "e2h" $
  parallel $ do
    it "parses storageJson" $ do
      let eitherContracts = do
            solNodes <- decodeSoleNodes (toS storageJson)
            let mContracts = mapM s2sContracts solNodes
            let contracts = concat $ runSimpleUniqueMonad mContracts
            return contracts
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
      let eitherContracts = do
            solNodes <- decodeSoleNodes (toS simpleDaoJson)
            let mContracts = mapM s2sContracts solNodes
            let contracts = concat $ runSimpleUniqueMonad mContracts
            return contracts
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
                          (JustId (Idfr "v1"))
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
                          (JustId (Idfr "v1"))
                          [ StAssign
                              (JustId (Idfr "v2"))
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
                              (JustId (Idfr "v3"))
                              (ExpCall (JustId (Idfr "v2")) [])
                          , StLocalVarDecl
                              (VarDecl
                               {vName = Idfr "res", vType = Unknown "bool"})
                          , StAssign
                              (JustId (Idfr "res"))
                              (ExpLval (JustId (Idfr "v3")))
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
                          (JustId (Idfr "v6"))
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
                          (ExpLval (JustId (Idfr "v6")))
                      ]
                  }
                , FunDefinition
                  { fName = Idfr ""
                  , fParams = []
                  , fReturns = []
                  , fBody =
                      [ StAssign
                          (JustId (Idfr "v4"))
                          (ExpCall
                             (Member
                              { mObj = JustId (Idfr "dao")
                              , mField = Idfr "queryCredit"
                              })
                             [JustId (Idfr "this")])
                      , StAssign
                          (JustId (Idfr "v5"))
                          (ExpCall
                             (Member
                              { mObj = JustId (Idfr "dao")
                              , mField = Idfr "withdraw"
                              })
                             [JustId (Idfr "v4")])
                      , StAssign
                          (JustId (Idfr "_"))
                          (ExpLval (JustId (Idfr "v5")))
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
                      [ StAssign (JustId (Idfr "v13")) (ExpLiteral "1")
                      , StAssign
                          (JustId (Idfr "v14"))
                          (ExpCall
                             (Member
                              { mObj =
                                  Member
                                  { mObj = JustId (Idfr "dao")
                                  , mField = Idfr "donate"
                                  }
                              , mField = Idfr "value"
                              })
                             [JustId (Idfr "v13")])
                      , StAssign
                          (JustId (Idfr "v15"))
                          (ExpCall (JustId (Idfr "v14")) [JustId (Idfr "this")])
                      , StAssign
                          (JustId (Idfr "_"))
                          (ExpLval (JustId (Idfr "v15")))
                      , StAssign (JustId (Idfr "v16")) (ExpLiteral "1")
                      , StAssign
                          (JustId (Idfr "v17"))
                          (ExpCall
                             (Member
                              { mObj = JustId (Idfr "dao")
                              , mField = Idfr "withdraw"
                              })
                             [JustId (Idfr "v16")])
                      , StAssign
                          (JustId (Idfr "_"))
                          (ExpLval (JustId (Idfr "v17")))
                      ]
                  }
                , FunDefinition
                  { fName = Idfr "getJackpot"
                  , fParams = []
                  , fReturns = []
                  , fBody =
                      [ StAssign
                          (JustId (Idfr "v10"))
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
                          (ExpLval (JustId (Idfr "v10")))
                      , StAssign
                          (JustId (Idfr "v11"))
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
                          (ExpLval (JustId (Idfr "v11")))
                      , StAssign (JustId (Idfr "v12")) (ExpLiteral "true")
                      , StAssign
                          (JustId (Idfr "performAttack"))
                          (ExpLval (JustId (Idfr "v12")))
                      ]
                  }
                , FunDefinition
                  { fName = Idfr ""
                  , fParams = []
                  , fReturns = []
                  , fBody =
                      [ StIf
                          (JustId (Idfr "performAttack"))
                          [ StAssign (JustId (Idfr "v7")) (ExpLiteral "false")
                          , StAssign
                              (JustId (Idfr "performAttack"))
                              (ExpLval (JustId (Idfr "v7")))
                          , StAssign (JustId (Idfr "v8")) (ExpLiteral "1")
                          , StAssign
                              (JustId (Idfr "v9"))
                              (ExpCall
                                 (Member
                                  { mObj = JustId (Idfr "dao")
                                  , mField = Idfr "withdraw"
                                  })
                                 [JustId (Idfr "v8")])
                          , StAssign
                              (JustId (Idfr "_"))
                              (ExpLval (JustId (Idfr "v9")))
                          ]
                          []
                      ]
                  }
                ]
            }
          ]
