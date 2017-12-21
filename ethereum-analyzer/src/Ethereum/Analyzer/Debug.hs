module Ethereum.Analyzer.Debug
  ( pprintContracts
  , pprintSimpleSol
  , prettyContracts
  , dbgGetSimpleSol
  ) where

import Protolude hiding (show)

import GHC.Show (Show(..))

import Ethereum.Analyzer.Solidity
import Text.PrettyPrint.Leijen.Text as PP hiding ((<$>))

_mergeEither :: Either t t -> t
_mergeEither (Right v) = v
_mergeEither (Left v) = v

dbgGetSimpleSol :: Text -> Either Text [Contract]
dbgGetSimpleSol = decodeContracts

pprintSimpleSol :: Text -> IO ()
pprintSimpleSol ast =
  putText $ _mergeEither $ (toS . prettyContracts) <$> (decodeContracts ast)

pprintContracts :: [Contract] -> IO ()
pprintContracts = putDoc . pretty

prettyContracts :: [Contract] -> Text
prettyContracts = toS . show . pretty
