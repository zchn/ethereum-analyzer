{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

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
  ) where

import Protolude hiding (show)

import Ethereum.Analyzer.Common
import Ethereum.Analyzer.Solidity.AstJson
import Text.PrettyPrint.Leijen.Text

import qualified Text.PrettyPrint as PP
import qualified Text.PrettyPrint.GenericPretty as GP

data Contract = Contract
  { cName :: Text
  , cStateVars :: [VarDecl]
  , cFunctions :: [FunDefinition]
  } deriving (Eq, Generic, Show, GP.Out)

data VarDecl = VarDecl
  { vName :: Idfr
  , vType :: VarType
  } deriving (Eq, Generic, Show, GP.Out)

newtype Idfr =
  Idfr Text
  deriving (Eq, Generic, Show, GP.Out)

data LValue
  = JustId Idfr
  | Index { iArray :: LValue
         ,  iIndex :: LValue}
  | Member { mObj :: LValue
          ,  mField :: Idfr}
  deriving (Eq, Generic, Show, GP.Out)

data VarType
  = Int256
  | Uint256
  | Address
  | Mapping VarType
            VarType
  | Unknown Text
  deriving (Eq, Generic, Show, GP.Out)

data FunDefinition = FunDefinition
  { fName :: Idfr
  , fParams :: [VarDecl]
  , fReturns :: [VarDecl]
  , fBody :: [Statement]
  } deriving (Eq, Generic, Show, GP.Out)

data Statement
  = StLocalVarDecl VarDecl
  | StAssign LValue
             Expression
  | StIf LValue
         [Statement]
         [Statement]
  | StReturn [LValue]
  | StTodo Text
  deriving (Eq, Generic, Show, GP.Out)

data Expression
  = ExpBin Text
           LValue
           LValue
  | ExpLiteral Text
  | ExpLval LValue
  | ExpCall LValue
            [LValue]
  deriving (Eq, Generic, Show, GP.Out)

instance GP.Out Text where
  doc = PP.quotes . PP.text . toS
  docPrec _ = GP.doc

instance GP.Out SolNode

s2sContracts :: SolNode -> [Contract]
s2sContracts SolNode {name = Just "SourceUnit", children = Just sChildren} =
  concat (map s2sContracts sChildren)
s2sContracts SolNode { name = Just "ContractDefinition"
                     , children = Just vChildren
                     , attributes = Just SolNode {name = Just cName}
                     } =
  let (vars, funs) = s2sVarsFuns vChildren
  in [Contract cName vars funs]
  where
    s2sVarsFuns :: [SolNode] -> ([VarDecl], [FunDefinition])
    s2sVarsFuns [] = ([], [])
    s2sVarsFuns (h:t) =
      let (vars', funs') = s2sVarsFuns t
      in (s2sVarDecls h <> vars', s2sFuns h <> funs')
s2sContracts _ = []

s2sVarDecls :: SolNode -> [VarDecl]
s2sVarDecls SolNode { name = Just "VariableDeclaration"
                    , attributes = Just SolNode { name = Just vName
                                                , _type = Just vType
                                                }
                    } = [VarDecl (Idfr vName) (Unknown vType)]
s2sVarDecls SolNode {name = Just "ParameterList", children = Just pChildren} =
  concat (map s2sVarDecls pChildren)
s2sVarDecls _ = []

s2sFuns :: SolNode -> [FunDefinition]
s2sFuns SolNode { name = Just "FunctionDefinition"
                , children = Just [params, returns, body]
                , attributes = Just SolNode {name = Just fName}
                } =
  [ FunDefinition
      (Idfr fName)
      (s2sVarDecls params)
      (s2sVarDecls returns)
      (s2sStatements body)
  ]
s2sFuns _ = []

s2sStatements :: SolNode -> [Statement]
s2sStatements SolNode {name = Just "Block", children = Just sChildren} =
  concat (map s2sStatements sChildren)
s2sStatements SolNode { name = Just "ExpressionStatement"
                      , children = Just sChildren
                      } = concat (map s2sStatements sChildren)
s2sStatements SolNode { name = Just "Assignment"
                      , children = Just [lval, rval]
                      , attributes = Just SolNode {operator = Just op}
                      } =
  let (prelval, simpleLval) = s2sLval lval
      (prerval, simpleRval) = s2sLval rval
  in prerval <> prelval <> [StAssign simpleLval $ ExpLval simpleRval]
s2sStatements e@SolNode {name = Just "Return", children = Just sChildren} =
  let presAndRvals = map s2sLval sChildren
      prerval = concat (map fst presAndRvals)
      simpleRvals = map snd presAndRvals
  in prerval <> [StReturn simpleRvals]
s2sStatements SolNode { name = Just "IfStatement"
                      , children = Just [cond, thenBr]
                      } =
  let (precond, lvalcond) = s2sLval cond
      thenSts = s2sStatements thenBr
  in precond <> [StIf lvalcond thenSts []]
s2sStatements SolNode { name = Just "IfStatement"
                      , children = Just [cond, thenBr, elseBr]
                      } =
  let (precond, lvalcond) = s2sLval cond
      thenSts = s2sStatements thenBr
      elseSts = s2sStatements elseBr
  in precond <> [StIf lvalcond thenSts elseSts]
s2sStatements SolNode { name = Just "VariableDeclarationStatement"
                      , children = Just [vdec@SolNode {name = Just "VariableDeclaration"}, vinit]
                      } =
  let varDecl@(VarDecl vId _):_ =
        s2sVarDecls vdec -- because VariableDeclaration only emits one decl
      (preDef, lvalDef) = s2sLval vinit
  in preDef <> [StLocalVarDecl varDecl] <>
     [StAssign (JustId vId) (ExpLval lvalDef)]
s2sStatements n@SolNode {name = Just "FunctionCall"} =
  let (precall, lvalcall) = s2sLval n
  in precall <> [StAssign (JustId $ Idfr "_") (ExpLval lvalcall)]
s2sStatements s = unimplementedPanic s {children = Nothing}

s2sLval :: SolNode -> ([Statement], LValue)
s2sLval SolNode { name = Just "Identifier"
                , attributes = Just SolNode {value = Just idName}
                } = ([], JustId (Idfr idName))
s2sLval SolNode { name = Just "MemberAccess"
                , children = Just [obj]
                , attributes = Just SolNode { _type = Just _
                                            , member_name = Just mName
                                            }
                } =
  let (prelval, simpleLval) = s2sLval obj
  in (prelval, Member simpleLval (Idfr mName))
s2sLval n@SolNode {name = Just "IndexAccess", children = Just (c1:ctail)} =
  let (prelval, simpleLval) = s2sLval c1
      (presub, simpleLvalSub) = handleSubscription simpleLval ctail
  in (presub <> prelval, Index simpleLval simpleLvalSub)
  where
    handleSubscription :: LValue -> [SolNode] -> ([Statement], LValue)
    handleSubscription lv [] = unexpectedPanic lv
    handleSubscription lv [subNode] =
      let (presub', simpleLvalSub') = s2sLval subNode
      in (presub', Index lv simpleLvalSub')
    handleSubscription lv (subNode:t) =
      let (presub', simpleLvalSub') = handleSubscription lv [subNode]
          (presub'', simpleLvalSub'') = handleSubscription simpleLvalSub' t
      in (presub'' <> presub', simpleLvalSub'')
s2sLval SolNode { name = Just "BinaryOperation"
                , children = Just [op1, op2]
                , attributes = Just SolNode {operator = Just vOp}
                } =
  let (preOp1, lvalOp1) = s2sLval op1
      (preOp2, lvalOp2) = s2sLval op2
  in ( preOp1 <> preOp2 <>
       [StAssign (JustId $ Idfr "TodoTmp") (ExpBin vOp lvalOp1 lvalOp2)]
     , JustId $ Idfr "TodoTmp")
s2sLval SolNode {name = Just "FunctionCall", children = Just (func:params)} =
  let (preFun, lvalFun) = s2sLval func
      preAndlvals = map s2sLval params
      preArgs = concat (map fst preAndlvals) -- TODO(zchn): reverse?
      lvalArgs = map snd preAndlvals
  in ( preArgs <> preFun <>
       [StAssign (JustId $ Idfr "TodoTmp") (ExpCall lvalFun lvalArgs)]
     , JustId $ Idfr "TodoTmp")
s2sLval SolNode { name = Just "Literal"
                , attributes = Just SolNode {value = Just vValue}
                } =
  ( [StAssign (JustId $ Idfr "TodotTmp") (ExpLiteral vValue)]
  , (JustId $ Idfr "TodoTmp"))
s2sLval n = unimplementedPanic n {children = Nothing}

-- lastLvalOf :: [Statement] -> LValue
-- lastLvalOf [_, StAssign lval _] = lval
-- lastLvalOf (_ : t) = lastLvalOf t
-- lastLvalOf _ = errorLValue
errorLValue = JustId (Idfr "ERROR!")
