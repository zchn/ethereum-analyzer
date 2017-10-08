module Ethereum.Analyzer.Solidity.Foreach
  ( statementsOf
  , expressionsOf
  ) where

import Protolude

import Data.List (concatMap)
import Ethereum.Analyzer.Common
import Ethereum.Analyzer.Solidity.Simple

statementsOf :: Contract -> [Statement]
statementsOf = _sOf

class HasStatements a where
  _sOf :: a -> [Statement]

instance HasStatements Contract where
  _sOf Contract {cFunctions = funs} = concatMap _sOf funs

instance HasStatements FunDefinition where
  _sOf FunDefinition {fBody = stmts} = concatMap _sOf stmts

instance HasStatements Statement where
  _sOf s@(StIf _ thenSts elseSts) =
    [s] <> concatMap _sOf thenSts <> concatMap _sOf elseSts
  _sOf s@(StLoop bodySts) = [s] <> concatMap _sOf bodySts
  _sOf s = [s]

expressionsOf :: Contract -> [Expression]
expressionsOf c = concatMap _eOf $ statementsOf c

_eOf :: Statement -> [Expression]
_eOf (StLocalVarDecl _) = []
_eOf (StAssign _ e) = [e]
_eOf (StIf _ _ _) = []
_eOf (StLoop _) = []
_eOf StBreak = []
_eOf (StReturn _) = []
_eOf (StDelete _) = []
_eOf st@(StTodo _) = unimplementedPanic st
_eOf StThrow = []
