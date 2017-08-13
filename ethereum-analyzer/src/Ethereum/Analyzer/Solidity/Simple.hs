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

import Ethereum.Analyzer.Solidity.AstJson
import Text.PrettyPrint.Leijen.Text as PP

data Contract = Contract
  { cName :: Text
  , cStateVars :: [VarDecl]
  , cFunctions :: [FunDefinition]
  } deriving (Eq, Generic, Show)

data VarDecl = VarDecl
  { vName :: Idfr
  , vType :: VarType
  } deriving (Eq, Generic, Show)

data Idfr = Idfr { iName :: Text }
  deriving (Eq, Generic, Show)

data LValue = JustId { idfr :: Idfr }
  | Index { iArray :: LValue
          , iIndex :: LValue }
  | Member { mObj :: LValue
           , mField :: Idfr }
            deriving (Eq, Generic, Show)

data VarType
  = Int256
  | Uint256
  | Address
  | Mapping VarType
            VarType
  | Unknown Text
 deriving (Eq, Generic, Show)

data FunDefinition = FunDefinition
  { fName :: Idfr
  , fParams :: [VarDecl]
  , fReturns :: [VarDecl]
  , fBody :: [Statement]
  } deriving (Eq, Generic, Show)

data Statement
  = StLocalVarDecl VarDecl
  | StAssign LValue
             Expression
  | StIf LValue
         [Statement]
         [Statement]
  | StReturn [LValue]
  | StTodo Text
  | Unsupported SolNode
 deriving (Eq, Generic, Show)

data Expression
  = BinOp Text
          LValue
          LValue
  | ExpLiteral Text
  | ExpLval LValue
 deriving (Eq, Generic, Show)

s2sContracts :: SolNode -> [Contract]
s2sContracts SolNode { name = Just "SourceUnit"
                      , children = Just sChildren } =
  concat (map s2sContracts sChildren)
s2sContracts SolNode { name = Just "ContractDefinition"
                     , children = Just vChildren
                     , attributes = Just SolNode { name = Just cName }} =
  let (vars, funs) = s2sVarsFuns vChildren in
    [Contract cName vars funs]
  where s2sVarsFuns :: [SolNode] -> ([VarDecl], [FunDefinition])
        s2sVarsFuns [] = ([], [])
        s2sVarsFuns (h : t) = let (vars', funs') = s2sVarsFuns t in
          (s2sVarDecls h <> vars', s2sFuns h <> funs')
s2sContracts _ = []

s2sVarDecls :: SolNode -> [VarDecl]
s2sVarDecls SolNode { name = Just "VariableDeclaration"
                    , attributes = Just SolNode { name = Just vName,
                                                  _type = Just vType }} = [VarDecl (Idfr vName) (Unknown vType)]
s2sVarDecls SolNode { name = Just "ParameterList"
                    , children = Just pChildren } = concat (map s2sVarDecls pChildren)
s2sVarDecls _ = []

s2sFuns :: SolNode -> [FunDefinition]
s2sFuns SolNode { name = Just "FunctionDefinition"
                , children = Just [ params, returns, body ]
                , attributes = Just SolNode { name = Just fName }} =
  [FunDefinition (Idfr fName)
    (s2sVarDecls params) (s2sVarDecls returns) (s2sStatements body)]
s2sFuns _ = []

s2sStatements :: SolNode -> [Statement]
s2sStatements SolNode { name = Just "Block"
                      , children = Just sChildren } =
  concat (map s2sStatements sChildren)
s2sStatements SolNode { name = Just "ExpressionStatement"
                        , children = Just sChildren } = concat (map s2sStatements sChildren)
s2sStatements SolNode { name = Just "Assignment"
                      , children = Just [lval, rval]
                      , attributes = Just SolNode { operator = Just op }} =
  let (prelval, simpleLval) = s2sLval lval
      (prerval, simpleRval) = s2sRval rval in
    prerval <> prelval <> [StAssign simpleLval simpleRval]
s2sStatements e@SolNode { name = Just "Return"
                        , children = Just sChildren } = [StTodo "Return"]
s2sStatements s = [Unsupported s]

s2sLval :: SolNode -> ([Statement], LValue)
s2sLval SolNode { name = Just "Identifier"
                 , attributes = Just SolNode { value = Just idName }}
  = ([], JustId (Idfr idName))
s2sLval n = ([Unsupported n], errorLValue)

s2sRval :: SolNode -> ([Statement], Expression)
s2sRval SolNode { name = Just "Identifier"
                 , attributes = Just SolNode { value = Just idName }}
  = ([], ExpLval $ JustId (Idfr idName))
s2sRval n = ([Unsupported n], ExpLval $ errorLValue)

lastLvalOf :: [Statement] -> LValue
lastLvalOf [_, StAssign lval _] = lval
lastLvalOf (_ : t) = lastLvalOf t
lastLvalOf _ = errorLValue

errorLValue = JustId (Idfr "ERROR!")
