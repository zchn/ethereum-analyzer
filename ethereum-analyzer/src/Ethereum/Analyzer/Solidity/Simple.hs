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
  , decodeContracts
  ) where

import Protolude hiding (show)

import Compiler.Hoopl
import Ethereum.Analyzer.Common
import Ethereum.Analyzer.Solidity.AstJson
import GHC.Show (Show(..))
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

import qualified Text.PrettyPrint as PP
import qualified Text.PrettyPrint.GenericPretty as GP

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
  | StDelete LValue
  | StTodo Text
  | StThrow
  deriving (Eq, Generic, Show, GP.Out)

data Expression
  = ExpUnary Text LValue
  | ExpBin Text
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

s2sContracts
  :: UniqueMonad m
  => SolNode -> m [Contract]
s2sContracts SolNode {_AST = Just n } = s2sContracts n
s2sContracts SolNode {name = Just "SourceUnit", children = Just sChildren} =
  concat <$> mapM s2sContracts sChildren
s2sContracts SolNode { name = Just "ContractDefinition"
                     , children = Just vChildren
                     , attributes = Just SolNode {name = Just cName}
                     } = do
  (vars, funs) <- s2sVarsFuns vChildren
  return [Contract cName vars funs]
  where
    s2sVarsFuns
      :: UniqueMonad m
      => [SolNode] -> m ([VarDecl], [FunDefinition])
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
                    } = [VarDecl (Idfr vName) (Unknown vType)]
s2sVarDecls SolNode {name = Just "ParameterList", children = Just pChildren} =
  concat (map s2sVarDecls pChildren)
s2sVarDecls _ = []

s2sFuns
  :: UniqueMonad m
  => SolNode -> m [FunDefinition]
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

s2sStatements
  :: UniqueMonad m
  => SolNode -> m [Statement]
s2sStatements SolNode {name = Just "Block", children = Just sChildren} =
  concat <$> (mapM s2sStatements sChildren)
s2sStatements SolNode { name = Just "ExpressionStatement"
                      , children = Just sChildren
                      } = concat <$> (mapM s2sStatements sChildren)
s2sStatements SolNode { name = Just "Assignment"
                      , children = Just [lval, rval]
                      , attributes = Just SolNode {operator = Just op}
                      } = do
  (prelval, simpleLval) <- s2sLval lval
  (prerval, simpleRval) <- s2sLval rval
  return $ prerval <> prelval <> [StAssign simpleLval $ ExpLval simpleRval]
s2sStatements e@SolNode {name = Just "Return", children = Just sChildren} = do
  presAndRvals <- mapM s2sLval sChildren
  let prerval = concat (map fst presAndRvals)
  let simpleRvals = map snd presAndRvals
  return $ prerval <> [StReturn simpleRvals]
s2sStatements e@SolNode { name = Just "UnaryOperation"
                , children = Just [op1]
                , attributes = Just SolNode {operator = Just "delete"}
                } = do
  (preOp1, lvalOp1) <- s2sLval op1
  newVar <- uniqueVar
  return $ preOp1 <> [StDelete (JustId $ Idfr newVar)]
s2sStatements e@SolNode { name = Just "UnaryOperation"
                , children = Just [
                    SolNode { name = Just "Identifier"
                            , attributes = Just SolNode { value = Just idName }}]
                , attributes = Just SolNode {operator = Just "++"}
                } = do
  let idfr = JustId $ Idfr idName
  newVar <- uniqueVar
  let newidfr = JustId $ Idfr newVar
  return [ StAssign newidfr $ ExpLiteral "1"
         , StAssign idfr $ ExpBin "+" idfr newidfr]
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
s2sStatements SolNode {name = Just "VariableDeclarationStatement"} =
  -- TODO(zchn): Handle this properly.
  return []
s2sStatements SolNode {name = Just "Throw"} = return [StThrow]
s2sStatements s = unimplementedPanic s {children = Nothing}

s2sLval
  :: UniqueMonad m
  => SolNode -> m ([Statement], LValue)
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
s2sLval n@SolNode {name = Just "IndexAccess", children = Just (c1:ctail)} = do
  (prelval, simpleLval) <- s2sLval c1
  (presub, simpleLvalSub) <- handleSubscription simpleLval ctail
  return (presub <> prelval, Index simpleLval simpleLvalSub)
  where
    handleSubscription
      :: UniqueMonad m
      => LValue -> [SolNode] -> m ([Statement], LValue)
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
    ( preOp1 <>
      [StAssign (JustId $ Idfr newVar) (ExpUnary vOp lvalOp1)]
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
s2sLval SolNode {name = Just "FunctionCall", children = Just (func:params)} = do
  (preFun, lvalFun) <- s2sLval func
  preAndlvals <- mapM s2sLval params
  let preArgs = concat (map fst preAndlvals) -- TODO(zchn): reverse?
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
                , attributes = Just SolNode { value = Just v }} =
  return ([], JustId $ Idfr v)
s2sLval n = unimplementedPanic n {children = Nothing}

uniqueVar
  :: UniqueMonad m
  => m Text
uniqueVar = ("v" <>) . toS . show <$> freshUnique

-- lastLvalOf :: [Statement] -> LValue
-- lastLvalOf [_, StAssign lval _] = lval
-- lastLvalOf (_ : t) = lastLvalOf t
-- lastLvalOf _ = errorLValue
errorLValue = JustId (Idfr "ERROR!")
