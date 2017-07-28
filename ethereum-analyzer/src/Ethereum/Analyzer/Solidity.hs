{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Ethereum.Analyzer.Solidity
  ( SolNode(..)
  , defSolNode
  ) where

import Protolude hiding (show)

import Data.Aeson
import Data.Aeson.Types

-- import Data.Default
-- import GHC.Generics
data SolNode = SolNode
  { children :: Maybe [SolNode]
  , _id :: Maybe Int
  , name :: Maybe Text
  , src :: Maybe Text
  , attributes :: Maybe SolNode
  , literals :: Maybe [Text]
  , _type :: Maybe Text
  , value :: Maybe Text
  , visibility :: Maybe Text
  , payable :: Maybe Bool
  , constant :: Maybe Bool
  , fullyImplemented :: Maybe Bool
  , isLibrary :: Maybe Bool
  , linearizedBaseContracts :: Maybe [Int]
  , storageLocation :: Maybe Text
  , operator :: Maybe Text
  } deriving (Eq, Generic, Show)

instance ToJSON SolNode where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = dropWhile (== '_') }

instance FromJSON SolNode where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = dropWhile (== '_') }

defSolNode :: SolNode
defSolNode =
  SolNode
  { children = Nothing
  , _id = Nothing
  , name = Nothing
  , src = Nothing
  , attributes = Nothing
  , literals = Nothing
  , _type = Nothing
  , value = Nothing
  , visibility = Nothing
  , payable = Nothing
  , constant = Nothing
  , fullyImplemented = Nothing
  , isLibrary = Nothing
  , linearizedBaseContracts = Nothing
  , storageLocation = Nothing
  , operator = Nothing
  }
