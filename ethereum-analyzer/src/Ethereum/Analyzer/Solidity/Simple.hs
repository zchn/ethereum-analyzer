{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Ethereum.Analyzer.Solidity.Simple
  ( Contract(..)
  , VarDecl(..)
  , Idfr(..)
  , LValue(..)
  , VarType(..)
  , FunDefinition(..)
  , Statement(..)
  , Expression(..)
  , s2sContracts
  , decodeContracts
  ) where

import Protolude hiding ((<>), show)

import Compiler.Hoopl
import Ethereum.Analyzer.Common
import Ethereum.Analyzer.Solidity.AstJson
import Ckev.In.Text
import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import qualified Data.Text as DT
-- import qualified Text.PrettyPrint.Leijen.Text as PP

decodeContracts :: Text -> Either Text [Contract]
decodeContracts astJsonText = do
  solNodes <- decodeSoleNodes (toS astJsonText)
  let mContracts = mapM s2sContracts solNodes
  let contracts = concat $ runSimpleUniqueMonad mContracts
  return contracts

data Contract = Contract
  { cName :: Text
  , cStateVars :: [VarDecl]
  , cFunctions :: [FunDefinition]
  } deriving (Eq, Generic, Show)

instance Pretty Contract where
  pretty Contract { cName = _name
                  , cStateVars = _statevars
                  , cFunctions = _functions
                  } =
    textStrict _name <+>
    braces (vsep $ map pretty _statevars <> map pretty _functions)

data VarDecl = VarDecl
  { vName :: Idfr
  , vType :: VarType
  } deriving (Eq, Generic, Show)

instance Pretty VarDecl where
  pretty VarDecl {vName = _name, vType = _type} = pretty _type <+> pretty _name

newtype Idfr = Idfr
  { unIdfr :: Text
  } deriving (Eq, Generic, Show)

instance Pretty Idfr where
  pretty = textStrict . unIdfr

data LValue
  = JustId Idfr
  | Index { iArray :: LValue
          , iIndex :: LValue }
  | Member { mObj :: LValue
           , mField :: Idfr }
  | Tuple [LValue]
  deriving (Eq, Generic, Show)

instance ShowText LValue where
  showText = showT

instance Pretty LValue where
  pretty (JustId v) = pretty v
  pretty (Index a i) = pretty a <> brackets (pretty i)
  pretty (Member o f) = pretty o <> textStrict "." <> pretty f
  pretty (Tuple l) = tupled (map pretty l)

data VarType
  = Int256
  | Uint256
  | Bool
  | Address
  | Mapping VarType
            VarType
  | Unknown Text
  deriving (Eq, Generic, Show)

instance Pretty VarType where
  pretty Int256 = textStrict "int256"
  pretty Uint256 = textStrict "uint256"
  pretty Bool = textStrict "bool"
  pretty Address = textStrict "address"
  pretty (Mapping k v) = pretty k <> textStrict "->" <> pretty v
  pretty (Unknown t) = textStrict ("unknown_" <> t)

data FunDefinition = FunDefinition
  { fName :: Idfr
  , fParams :: [VarDecl]
  , fReturns :: [VarDecl]
  , fBody :: [Statement]
  } deriving (Eq, Generic, Show)

instance Pretty FunDefinition where
  pretty FunDefinition { fName = _name
                       , fParams = _params
                       , fReturns = _returns
                       , fBody = _body
                       } =
    pretty _name <> tupled (map pretty _params) <+>
    textStrict "returns" <> tupled (map pretty _returns) <+>
    semiBraces (map pretty _body)

data Statement
  = StLocalVarDecl VarDecl
  | StAssign LValue
             Expression
  | StIf LValue
         [Statement]
         [Statement]
  | StLoop [Statement]
  | StBreak
  | StContinue
  | StReturn [LValue]
  | StDelete LValue
  | StTodo Text
  | StThrow
  deriving (Eq, Generic, Show)

instance Pretty Statement where
  pretty (StLocalVarDecl vd) = pretty vd
  pretty (StAssign lv exp) = pretty lv <+> textStrict "=" <+> pretty exp
  pretty (StIf cond thenB elseB) =
    textStrict "if" <+>
    parens (pretty cond) <+>
    semiBraces (map pretty thenB) <+>
    textStrict "else" <+> semiBraces (map pretty elseB)
  pretty (StLoop loopB) = textStrict "loop" <+> semiBraces (map pretty loopB)
  pretty (StBreak) = textStrict "break"
  pretty (StContinue) = textStrict "continue"
  pretty (StReturn rvals) = textStrict "return" <+> tupled (map pretty rvals)
  pretty (StDelete v) = textStrict "delete" <+> pretty v
  pretty (StTodo t) = textStrict "todo" <+> textStrict t
  pretty (StThrow) = textStrict "throw"

data Expression
  = ExpUnary Text
             LValue
  | ExpBin Text
           LValue
           LValue
  | ExpLiteral Text
  | ExpLval LValue
  | ExpCall LValue
            [LValue]
  deriving (Eq, Generic, Show)

instance Pretty Expression where
  pretty (ExpUnary op v) = textStrict op <> pretty v
  pretty (ExpBin op v1 v2) = pretty v1 <> textStrict op <> pretty v2
  pretty (ExpLiteral v) = pretty v
  pretty (ExpLval lv) = pretty lv
  pretty (ExpCall f lvals) = pretty f <> tupled (map pretty lvals)

s2sContracts :: UniqueMonad m => SolNode -> m [Contract]
s2sContracts SolNode {_AST = Just n} = s2sContracts n
s2sContracts SolNode {name = Just "SourceUnit", children = Just sChildren} =
  concat <$> mapM s2sContracts sChildren
s2sContracts SolNode { name = Just "ContractDefinition"
                     , children = Just vChildren
                     , attributes = Just SolNode {name = Just cName}
                     } = do
  (vars, funs) <- s2sVarsFuns vChildren
  return [Contract cName vars funs]
  where
    s2sVarsFuns :: UniqueMonad m => [SolNode] -> m ([VarDecl], [FunDefinition])
    s2sVarsFuns [] = return ([], [])
    s2sVarsFuns (h:t) = do
      (vars', funs') <- s2sVarsFuns t
      hFuns <- s2sFuns h
      return (s2sVarDecls h <> vars', hFuns <> funs')
s2sContracts _ = return []

-- s2sContracts n = unexpectedPanic n
s2sVarDecls :: SolNode -> [VarDecl]
s2sVarDecls SolNode { name = Just "VariableDeclaration"
                    , attributes = Just SolNode { name = Just vName
                                                , _type = Just vType
                                                }
                    } =
  [ VarDecl
      (Idfr vName)
      (case vType of
         "bool" -> Bool
         "address" -> Address
         "int256" -> Int256
         "uint256" -> Uint256
         _ -> Unknown vType)
  ]
s2sVarDecls SolNode {name = Just "ParameterList", children = Just pChildren} =
  concatMap s2sVarDecls pChildren
s2sVarDecls _ = []

s2sFuns :: UniqueMonad m => SolNode -> m [FunDefinition]
s2sFuns SolNode { name = Just "FunctionDefinition"
                , children = Just [params, returns, body]
                , attributes = Just SolNode {name = Just fName}
                } = do
  sBody <- s2sStatements body
  return
    [ FunDefinition
        (Idfr fName)
        (s2sVarDecls params)
        (s2sVarDecls returns)
        sBody
    ]
s2sFuns _ = return []

s2sStatements :: UniqueMonad m => SolNode -> m [Statement]
s2sStatements SolNode {name = Just "Block", children = Just sChildren} =
  concat <$> mapM s2sStatements sChildren
s2sStatements SolNode { name = Just "ExpressionStatement"
                      , children = Just sChildren
                      } = concat <$> mapM s2sStatements sChildren
s2sStatements SolNode { name = Just "Assignment"
                      , children = Just [lval, rval]
                      , attributes = Just SolNode {operator = Just _}
                      } = do
  (prelval, simpleLval) <- s2sLval lval
  (prerval, simpleRval) <- s2sLval rval
  return $ prerval <> prelval <> [StAssign simpleLval $ ExpLval simpleRval]
s2sStatements SolNode {name = Just "Return", children = ch} = do
  let sChildren = fromMaybe [] ch
  presAndRvals <- mapM s2sLval sChildren
  let prerval = concatMap fst presAndRvals
  let simpleRvals = map snd presAndRvals
  return $ prerval <> [StReturn simpleRvals]
s2sStatements SolNode { name = Just "UnaryOperation"
                        , children = Just [op1]
                        , attributes = Just SolNode {operator = Just "delete"}
                        } = do
  (preOp1, lvalOp1) <- s2sLval op1
  return $ preOp1 <> [StDelete lvalOp1]
s2sStatements SolNode { name = Just "UnaryOperation"
                        , children = Just [SolNode { name = Just "Identifier"
                                                   , attributes = Just SolNode {value = Just idName}
                                                   }]
                        , attributes = Just SolNode {operator = Just "++"}
                        } = do
  let idfr = JustId $ Idfr idName
  newVar <- uniqueVar
  let newidfr = JustId $ Idfr newVar
  return
    [StAssign newidfr $ ExpLiteral "1", StAssign idfr $ ExpBin "+" idfr newidfr]
s2sStatements SolNode { name = Just "UnaryOperation"
                        , children = Just [op1]
                        , attributes = Just SolNode {operator = Just "++"}
                        } = do
  (preOp1, lvalOp1) <- s2sLval op1
  newVar <- uniqueVar
  let newidfr = JustId $ Idfr newVar
  return $
    preOp1 <>
    [ StAssign newidfr $ ExpLiteral "1"
    , StAssign lvalOp1 $ ExpBin "+" lvalOp1 newidfr
    ]
s2sStatements SolNode { name = Just "UnaryOperation"
                      , children = Just [SolNode { name = Just "Identifier"
                                                 , attributes = Just SolNode {value = Just idName}
                                                 }]
                      , attributes = Just SolNode {operator = Just "--"}
                      } = do
  let idfr = JustId $ Idfr idName
  newVar <- uniqueVar
  let newidfr = JustId $ Idfr newVar
  return
    [StAssign newidfr $ ExpLiteral "1", StAssign idfr $ ExpBin "-" idfr newidfr]
s2sStatements SolNode { name = Just "UnaryOperation"
                        , children = Just [op1]
                        , attributes = Just SolNode {operator = Just "--"}
                        } = do
  (preOp1, lvalOp1) <- s2sLval op1
  newVar <- uniqueVar
  let newidfr = JustId $ Idfr newVar
  return $
    preOp1 <>
    [ StAssign newidfr $ ExpLiteral "1"
    , StAssign lvalOp1 $ ExpBin "-" lvalOp1 newidfr
    ]
s2sStatements SolNode { name = Just "IfStatement"
                      , children = Just [cond, thenBr]
                      } = do
  (precond, lvalcond) <- s2sLval cond
  thenSts <- s2sStatements thenBr
  return $ precond <> [StIf lvalcond thenSts []]
s2sStatements SolNode { name = Just "IfStatement"
                      , children = Just [cond, thenBr, elseBr]
                      } = do
  (precond, lvalcond) <- s2sLval cond
  thenSts <- s2sStatements thenBr
  elseSts <- s2sStatements elseBr
  return $ precond <> [StIf lvalcond thenSts elseSts]
s2sStatements SolNode { name = Just "WhileStatement"
                      , children = Just [cond, body]
                      } = do
  (precond, lvalcond) <- s2sLval cond
  bodySts <- s2sStatements body
  return [StLoop (precond <> [StIf lvalcond (bodySts <> [StContinue]) [StBreak]])]
s2sStatements SolNode { name = Just "DoWhileStatement"
                      , children = Just [cond, body]
                      } = do
  (precond, lvalcond) <- s2sLval cond
  bodySts <- s2sStatements body
  return [StLoop (bodySts <> precond <> [StIf lvalcond [StContinue] [StBreak]])]
s2sStatements SolNode { name = Just "ForStatement"
                      , children = Just [inits, cond, iters, body]
                      } = do
  initSts <- s2sStatements inits
  (precond, lvalcond) <- s2sLval cond
  iterSts <- s2sStatements iters
  bodySts <- s2sStatements body
  return $
    initSts <>
    [StLoop (precond <> [StIf lvalcond (bodySts <> iterSts <> [StContinue]) [StBreak]])]
s2sStatements SolNode {name = Just "Break"} = return [StBreak]
s2sStatements SolNode {name = Just "Continue"} = return [StContinue]
s2sStatements SolNode { name = Just "VariableDeclarationStatement"
                      , children = Just [vdec@SolNode {name = Just "VariableDeclaration"}, vinit]
                      } = do
  let varDecl@(VarDecl vId _):_ =
        s2sVarDecls vdec -- because VariableDeclaration only emits one decl
  (preDef, lvalDef) <- s2sLval vinit
  return $
    preDef <> [StLocalVarDecl varDecl] <>
    [StAssign (JustId vId) (ExpLval lvalDef)]
s2sStatements n@SolNode {name = Just "FunctionCall"} = do
  (precall, lvalcall) <- s2sLval n
  return $ precall <> [StAssign (JustId $ Idfr "_") (ExpLval lvalcall)]
s2sStatements SolNode {name = Just "VariableDeclarationStatement"}
  -- TODO(zchn): Handle this properly.
 = return []
s2sStatements SolNode {name = Just "Throw"} = return [StThrow]
s2sStatements SolNode {name = Just "InlineAssembly"} =
  return [StTodo "InlineAssembly"]
s2sStatements s = unimplementedPanic s

s2sLval :: UniqueMonad m => SolNode -> m ([Statement], LValue)
s2sLval SolNode { name = Just "Identifier"
                , attributes = Just SolNode {value = Just idName}
                } = return ([], JustId (Idfr idName))
s2sLval SolNode { name = Just "MemberAccess"
                , children = Just [obj]
                , attributes = Just SolNode { _type = Just _
                                            , member_name = Just mName
                                            }
                } = do
  (prelval, simpleLval) <- s2sLval obj
  return (prelval, Member simpleLval (Idfr mName))
s2sLval SolNode {name = Just "IndexAccess", children = Just (c1:ctail)} = do
  (prelval, simpleLval) <- s2sLval c1
  (presub, simpleLvalSub) <- handleSubscription simpleLval ctail
  return (presub <> prelval, Index simpleLval simpleLvalSub)
  where
    handleSubscription ::
         UniqueMonad m => LValue -> [SolNode] -> m ([Statement], LValue)
    handleSubscription lv [] = unexpectedPanic lv
    handleSubscription lv [subNode] = do
      (presub', simpleLvalSub') <- s2sLval subNode
      return (presub', Index lv simpleLvalSub')
    handleSubscription lv (subNode:t) = do
      (presub', simpleLvalSub') <- handleSubscription lv [subNode]
      (presub'', simpleLvalSub'') <- handleSubscription simpleLvalSub' t
      return (presub'' <> presub', simpleLvalSub'')
s2sLval SolNode { name = Just "UnaryOperation"
                , children = Just [op1]
                , attributes = Just SolNode {operator = Just vOp}
                } = do
  (preOp1, lvalOp1) <- s2sLval op1
  newVar <- uniqueVar
  return
    ( preOp1 <> [StAssign (JustId $ Idfr newVar) (ExpUnary vOp lvalOp1)]
    , JustId $ Idfr newVar)
s2sLval SolNode { name = Just "BinaryOperation"
                , children = Just [op1, op2]
                , attributes = Just SolNode {operator = Just vOp}
                } = do
  (preOp1, lvalOp1) <- s2sLval op1
  (preOp2, lvalOp2) <- s2sLval op2
  newVar <- uniqueVar
  return
    ( preOp1 <> preOp2 <>
      [StAssign (JustId $ Idfr newVar) (ExpBin vOp lvalOp1 lvalOp2)]
    , JustId $ Idfr newVar)
s2sLval SolNode { name = Just "Conditional"
                , children = Just [cond, opThen, opElse]
                } = do
  (preCond, lvalCond) <- s2sLval cond
  (preOpThen, lvalOpThen) <- s2sLval opThen
  (preOpElse, lvalOpElse) <- s2sLval opElse
  opVar <- uniqueVar
  return
    ( preCond <>
      [ StIf
          lvalCond
          (preOpThen <> [StAssign (JustId $ Idfr opVar) (ExpLval lvalOpThen)])
          (preOpElse <> [StAssign (JustId $ Idfr opVar) (ExpLval lvalOpElse)])
      ]
    , JustId $ Idfr opVar)
s2sLval SolNode {name = Just "FunctionCall", children = Just (func:params)} = do
  (preFun, lvalFun) <- s2sLval func
  preAndlvals <- mapM s2sLval params
  let preArgs = concatMap fst preAndlvals -- TODO(zchn): reverse?
  let lvalArgs = map snd preAndlvals
  newVar <- uniqueVar
  return
    ( preArgs <> preFun <>
      [StAssign (JustId $ Idfr newVar) (ExpCall lvalFun lvalArgs)]
    , JustId $ Idfr newVar)
s2sLval SolNode { name = Just "Literal"
                , attributes = Just SolNode {value = Just vValue}
                } = do
  newVar <- uniqueVar
  return
    ( [StAssign (JustId $ Idfr newVar) (ExpLiteral vValue)]
    , JustId $ Idfr newVar)
s2sLval SolNode { name = Just "ElementaryTypeNameExpression"
                , attributes = Just SolNode {value = Just v}
                } = return ([], JustId $ Idfr v)
-- For TupleExpression @ solc-0.4.11
s2sLval SolNode {name = Just "TupleExpression", children = Just elems} = do
  preAndlvals <- mapM s2sLval elems
  let preArgs = concatMap fst preAndlvals -- TODO(zchn): reverse?
  let lvalArgs = map snd preAndlvals
  return (preArgs, Tuple lvalArgs)
-- For TupleExpression @ solc-0.4.17
s2sLval SolNode { name = Just "TupleExpression"
                , attributes = Just SolNode {components = Just maybeComps}
                } = do
  let comps = catMaybes maybeComps
  preAndlvals <- mapM s2sLval comps
  let preArgs = concatMap fst preAndlvals -- TODO(zchn): reverse?
  let lvalArgs = map snd preAndlvals
  return (preArgs, Tuple lvalArgs)
s2sLval SolNode { name = Just "NewExpression"
                , attributes = Just SolNode {_type = Just t}
                } = do
  let normalized = "_ea_new_" <> DT.replace t " " "_"
  return ([], JustId $ Idfr normalized)
s2sLval n = unimplementedPanic n {children = Nothing}

uniqueVar :: UniqueMonad m => m Text
uniqueVar = ("v" <>) . toS . showT <$> freshUnique

-- lastLvalOf :: [Statement] -> LValue
-- lastLvalOf [_, StAssign lval _] = lval
-- lastLvalOf (_ : t) = lastLvalOf t
-- lastLvalOf _ = errorLValue
-- errorLValue = JustId (Idfr "ERROR!")
