module Ethereum.Analyzer.Solidity.Foreach
    ( statementsOf
    , expressionsOf
    ) where

import           Ethereum.Analyzer.Common
import           Ethereum.Analyzer.Solidity.Simple

statementsOf :: Contract -> [Statement]
statementsOf c = unimplementedPanic c

expressionsOf :: Contract -> [Expression]
expressionsOf c = unimplementedPanic c
