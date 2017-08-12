module Ethereum.Analyzer.Solidity.Simple
  ( Contract(..)
  , VarDecl(..)
  , Idfr(..)
  , VarType(..)
  , FunDefinition(..)
  , Statement(..)
  , Expression(..)) where

import Protolude hiding (show)

data Contract = Contract
 { cName :: Text
 , cStateVars :: [VarDecl]
 , cFunctions :: [FunDefinition] }

data VarDecl = VarDecl
  { vName :: Idfr
  , vType :: VarType }

data Idfr = Idfr
  { iName :: Text}

data VarType = Int256 | Uint256 | Address | Mapping VarType VarType

data FunDefinition = FunDefinition
  { fName :: Idfr
  , fParams :: [VarDecl]
  , fReturns :: [VarDecl]
  , fBody :: [Statement]}

data Statement = StLocalVarDecl VarDecl
               | StAssign Idfr Expression
               | StIf Idfr [Statement] [Statement]

data Expression = BinOp Text Idfr Idfr
                | ExpLiteral Text
