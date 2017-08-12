{-# LANGUAGE DeriveGeneric #-}

module Ethereum.Analyzer.Solidity.AstJson
  ( SolNode(..)
  , defSolNode
  ) where

import Protolude hiding (show)

import Data.Aeson
import Data.Aeson.Types
import Ethereum.Analyzer.Common
import GHC.Show (Show(..))

-- import Data.Default
-- import GHC.Generics
data SolNode = SolNode
  { name :: Maybe Text
  , _id :: Maybe Int
  , _type :: Maybe Text
  , attributes :: Maybe SolNode
  , constant :: Maybe Bool
  , fullyImplemented :: Maybe Bool
  , hexvalue :: Maybe Text
  , isLibrary :: Maybe Bool
  , linearizedBaseContracts :: Maybe [Int]
  , literals :: Maybe [Text]
  , member_name :: Maybe Text
  , operator :: Maybe Text
  , payable :: Maybe Bool
  , src :: Maybe Text
  , storageLocation :: Maybe Text
  , subdenomination :: Maybe Text
  , token :: Maybe Text
  , type_conversion :: Maybe Bool
  , value :: Maybe Text
  , visibility :: Maybe Text
  , children :: Maybe [SolNode]
  } deriving (Eq, Generic, Show)

instance ToJSON SolNode where
  toJSON =
    genericToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')}

instance FromJSON SolNode where
  parseJSON =
    genericParseJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')}

defSolNode :: SolNode
defSolNode =
  SolNode
  { children = Nothing
  , _id = Nothing
  , _type = Nothing
  , attributes = Nothing
  , constant = Nothing
  , fullyImplemented = Nothing
  , hexvalue = Nothing
  , isLibrary = Nothing
  , linearizedBaseContracts = Nothing
  , literals = Nothing
  , member_name = Nothing
  , name = Nothing
  , operator = Nothing
  , payable = Nothing
  , src = Nothing
  , storageLocation = Nothing
  , subdenomination = Nothing
  , token = Nothing
  , type_conversion = Nothing
  , value = Nothing
  , visibility = Nothing
  }
