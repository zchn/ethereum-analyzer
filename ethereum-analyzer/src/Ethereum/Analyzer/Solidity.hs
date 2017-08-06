{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Ethereum.Analyzer.Solidity
  ( SolNode(..)
  , defSolNode
  ) where

import Protolude hiding (show)

import Data.Aeson
import Data.Aeson.Types
import GHC.Show (Show(..))
import Text.PrettyPrint.Leijen.Text as PP

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

instance Pretty SolNode where
  pretty n@SolNode { name = name, children = children }
    | name == Just "SourceUnit" = braces $ pretty children
    | name == Just "PragmaDirective" = PP.empty
    | name == Just "ContractDefinition" = prettyContractDefinition n
    | name == Just "VariableDeclaration" = prettyVariableDeclaration n
    | name == Just "FunctionDefinition" = prettyFunctionDefinition n
    | name == Just "ParameterList" = prettyParameterList n
    | name == Just "Block" = prettyBlock n
    | name == Just "ExpressionStatement" = prettyExpressionStatement n
    | name == Just "Assignment" = prettyAssignment n
    | name == Just "Identifier" = prettyIdentifier n
    | name == Just "Return" = prettyReturn n
    | otherwise = panic . toS $ "unimplemented: " ++ showWithoutChildren n

showWithoutChildren n = show (n { children = Nothing})

prettyContractDefinition n@SolNode
  { name = Just "ContractDefinition"
  , children = Just children
  , attributes = Just (
      SolNode { children = Nothing
              , name = Just cName})} = textStrict "contract" </> textStrict cName <> (
                                       braces $ pretty children)
prettyContractDefinition n = unexpectedPanic n

prettyVariableDeclaration n@SolNode
  { name = Just "VariableDeclaration"
  , children = Just [ SolNode
                      { children = Nothing
                      , name = Just "ElementaryTypeName"
                      , attributes = Just SolNode
                        { name = Just veType }
                      } ]
  , attributes = Just (
      SolNode { children = Nothing
              , name = Just vName
              , _type = Just vType })} = textStrict (vType <> "/" <> veType) <>
                                         textStrict vName
prettyVariableDeclaration n = unexpectedPanic n

prettyFunctionDefinition n@SolNode
  { name = Just "FunctionDefinition"
  , children = Just fChildren
  , attributes = Just (
      SolNode { children = Nothing
              , name = Just fName })} = textStrict "fun" </> textStrict fName <> pretty fChildren
prettyFunctionDefinition n = unexpectedPanic n

prettyParameterList n@SolNode
  { name = Just "ParameterList"
  , children = Just pChildren } = parens $ pretty pChildren
prettyParameterList n = unexpectedPanic n

prettyBlock n@SolNode
  { name = Just "Block"
  , children = Just children } = braces $ pretty children
prettyBlock n = unexpectedPanic n

prettyExpressionStatement n@SolNode
  { name = Just "ExpressionStatement"
  , children = Just children } = parens $ pretty children
prettyExpressionStatement n = unexpectedPanic n

prettyAssignment n@SolNode
  { name = Just "Assignment"
  , children = Just children
  , attributes = Just (
      SolNode { _type = Just _type
              , operator = Just operator })} =
  parens (textStrict _type) <> cat (punctuate (textStrict operator) (
                                       map pretty children))
prettyAssignment n = unexpectedPanic n

prettyIdentifier n@SolNode
  { name = Just "Identifier"
  , children = Nothing
  , attributes = Just (
      SolNode { _type = Just _type
              , value = Just idName })} = textStrict (idName <> ":" <> _type)
prettyIdentifier n = unexpectedPanic n

prettyReturn n@SolNode
  { name = Just "Return"
  , children = Just children } = textStrict "return" <> parens (pretty children)
prettyReturn n = unexpectedPanic n

unexpectedPanic n = panic . toS $ "unexpected: " ++ show n

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
