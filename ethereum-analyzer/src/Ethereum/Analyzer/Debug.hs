module Ethereum.Analyzer.Debug
  ( pprintSimpleSol
  , dbgGetSimpleSol
  ) where

import Protolude

import Compiler.Hoopl
import Data.Aeson
import Ethereum.Analyzer.Common
import Ethereum.Analyzer.Solidity
import qualified Text.PrettyPrint.GenericPretty as GP

dbgGetSimpleSol :: Text -> Either Text [Contract]
dbgGetSimpleSol astJsonText =
  runSimpleUniqueMonad . s2sContracts <$>
  (s2t4Either $ eitherDecode (toS astJsonText))

pprintSimpleSol :: Text -> IO ()
pprintSimpleSol = GP.pp . dbgGetSimpleSol
