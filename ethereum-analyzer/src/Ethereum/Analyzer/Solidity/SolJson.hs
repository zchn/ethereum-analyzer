{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Ethereum.Analyzer.Solidity.SolJson
  ( decodeAst
  ) where

import Protolude hiding (show)

import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Lazy hiding (map)
import Ethereum.Analyzer.Common
import Ethereum.Analyzer.Solidity.AstJson
import GHC.Show (Show(..))

decodeAst :: LByteString -> Either Text [SolNode]
decodeAst combined_ast = do
  value <- (s2t4Either (eitherDecode combined_ast) :: Either Text Value)
  case value of
    Object o1 -> do
      srcObj <- (maybeToRight "Could not find 'sources' in object"
                  (lookup "sources" o1) :: Either Text Value)
      case srcObj of
        Object o2 -> do
          let srcUnitObjs = elems o2
          mapM (s2t4Either . eitherDecode . encode) (srcUnitObjs :: [Value])
        _ -> Left "'sources''s value is not an Aeson Object"
    _ -> Left "input is not an Aeson Object"
