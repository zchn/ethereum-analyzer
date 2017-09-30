module Ethereum.Analyzer.Debug
  ( pprintSimpleSol
  , dbgGetSimpleSol
  ) where

import Protolude

import Compiler.Hoopl
import Ethereum.Analyzer.Common
import Ethereum.Analyzer.Solidity
import qualified Text.PrettyPrint.GenericPretty as GP

dbgGetSimpleSol :: Text -> Either Text [Contract]
dbgGetSimpleSol astJsonText = do
  solNodes <- decodeAst (toS astJsonText)
  let mContracts = mapM s2sContracts solNodes
  let contracts = concat $ runSimpleUniqueMonad mContracts
  return contracts

pprintSimpleSol :: Text -> IO ()
pprintSimpleSol = GP.pp . dbgGetSimpleSol
