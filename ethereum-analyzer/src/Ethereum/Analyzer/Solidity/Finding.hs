module Ethereum.Analyzer.Solidity.Finding
  ( findingsFor
  ) where

import Protolude

import Ethereum.Analyzer.Solidity.Foreach
import Ethereum.Analyzer.Solidity.Simple

findingsFor :: Contract -> [Text]
findingsFor = selfdestruct

selfdestruct :: Contract -> [Text]
selfdestruct c = concatMap sdExp $ expressionsOf c
  where
    sdExp exp =
      case exp of
        ExpCall (JustId (Idfr "suicide")) _ ->
          ["Prefer 'selfdestruct' over 'suicide'."]
        _ -> []
