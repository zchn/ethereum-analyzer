module Ethereum.Analyzer.Debug
  ( pprintContracts
  , pprintSimpleSol
  , prettyContracts
  , dbgGetSimpleSol
  ) where

import Protolude

import Ethereum.Analyzer.Solidity
import qualified Text.PrettyPrint.GenericPretty as GP

dbgGetSimpleSol :: Text -> Either Text [Contract]
dbgGetSimpleSol = decodeContracts

pprintSimpleSol :: Text -> IO ()
pprintSimpleSol = GP.pp . dbgGetSimpleSol

pprintContracts :: [Contract] -> IO ()
pprintContracts = GP.pp

prettyContracts :: [Contract] -> Text
prettyContracts = toS . GP.pretty
