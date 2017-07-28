{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Ethereum.Analyzer.Solidity
  ( SolNode(..)
  , defSolNode
  ) where

import Protolude hiding (show, id)

import Data.Aeson

-- import Data.Default
-- import GHC.Generics
data SolNode = SolNode
  { children :: Maybe [SolNode]
  , id :: Maybe Int
  , name :: Maybe Text
  , src :: Maybe Text
  , attributes :: Maybe SolNode
  , literals :: Maybe [Text]
  } deriving (Eq, Generic, Show, FromJSON, ToJSON)

defSolNode :: SolNode
defSolNode =
  SolNode
  { children = Nothing
  , id = Nothing
  , name = Nothing
  , src = Nothing
  , attributes = Nothing
  , literals = Nothing
  }
