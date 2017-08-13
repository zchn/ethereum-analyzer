{-# LANGUAGE DeriveGeneric #-}

module Ethereum.Analyzer.Solidity.Simple
  ( Contract(..)
  , VarDecl(..)
  , Idfr(..)
  , VarType(..)
  , FunDefinition(..)
  , Statement(..)
  , Expression(..)
  , s2sContracts
  ) where

import Protolude hiding (show)

import Ethereum.Analyzer.Solidity.AstJson

data Contract = Contract
  { cName :: Text
  , cStateVars :: [VarDecl]
  , cFunctions :: [FunDefinition]
  } deriving (Eq, Generic, Show)

data VarDecl = VarDecl
  { vName :: Idfr
  , vType :: VarType
  } deriving (Eq, Generic, Show)

data Idfr = Idfr
  { iName :: Text
  } deriving (Eq, Generic, Show)

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
  | StAssign Idfr
             Expression
  | StIf Idfr
         [Statement]
         [Statement]
  | Unsupported SolNode
 deriving (Eq, Generic, Show)

data Expression
  = BinOp Text
          Idfr
          Idfr
  | ExpLiteral Text
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
s2sStatements e@SolNode { name = Just "ExpressionStatement"
                        , children = Just sChildren } = [Unsupported e]
s2sStatements s = [Unsupported s]
