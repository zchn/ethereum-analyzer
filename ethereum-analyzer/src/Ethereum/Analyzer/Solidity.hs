{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Ethereum.Analyzer.Solidity
  ( SolNode(..)
  , defSolNode
  ) where

import Protolude hiding (show)

import Data.Aeson
import Data.Aeson.Types
import Ethereum.Analyzer.Common
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
  toJSON =
    genericToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')}

instance FromJSON SolNode where
  parseJSON =
    genericParseJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')}

instance Pretty SolNode where
  pretty n@SolNode {name = name}
    | name == Just "SourceUnit" = prettySourceUnit n
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
    | otherwise = panic . toS $ "unimplemented: " <> showWithoutChildren n

showWithoutChildren :: SolNode -> Text
showWithoutChildren n = toS $ show (n {children = Nothing})

prettySourceUnit :: SolNode -> Doc
prettySourceUnit SolNode {name = Just "SourceUnit", children = Just children} =
  textStrict "//--SourceUnit--" PP.<$> vsep (map pretty children)
prettySourceUnit n = unexpectedPanic n

prettyContractDefinition :: SolNode -> Doc
prettyContractDefinition SolNode { name = Just "ContractDefinition"
                                 , children = Just children
                                 , attributes = Just (SolNode { children = Nothing
                                                              , name = Just cName
                                                              })
                                 } =
  textStrict "contract" </> textStrict cName </>
  semiBraces (map pretty children)
prettyContractDefinition n = unexpectedPanic n

prettyVariableDeclaration :: SolNode -> Doc
prettyVariableDeclaration SolNode { name = Just "VariableDeclaration"
                                  , children = Just [SolNode { children = Nothing
                                                             , name = Just "ElementaryTypeName"
                                                             , attributes = Just SolNode {name = Just veType}
                                                             }]
                                  , attributes = Just (SolNode { children = Nothing
                                                               , name = Just vName
                                                               , _type = Just vType
                                                               })
                                  } =
  textStrict (vType <> "/" <> veType) </> textStrict vName
prettyVariableDeclaration n = unexpectedPanic n

prettyFunctionDefinition :: SolNode -> Doc
prettyFunctionDefinition SolNode { name = Just "FunctionDefinition"
                                 , children = Just fChildren
                                 , attributes = Just (SolNode { children = Nothing
                                                              , name = Just fName
                                                              })
                                 } =
  textStrict "fun" </> align (textStrict fName </> cat (map pretty fChildren))
prettyFunctionDefinition n = unexpectedPanic n

prettyParameterList :: SolNode -> Doc
prettyParameterList SolNode { name = Just "ParameterList"
                            , children = Just pChildren
                            } =
  parens (align (cat (punctuate comma $ map pretty pChildren)))
prettyParameterList n = unexpectedPanic n

prettyBlock :: SolNode -> Doc
prettyBlock SolNode {name = Just "Block", children = Just children} =
  semiBraces $ map pretty children
prettyBlock n = unexpectedPanic n

prettyExpressionStatement :: SolNode -> Doc
prettyExpressionStatement SolNode { name = Just "ExpressionStatement"
                                  , children = Just children
                                  } = tupled $ map pretty children
prettyExpressionStatement n = unexpectedPanic n

prettyAssignment :: SolNode -> Doc
prettyAssignment SolNode { name = Just "Assignment"
                         , children = Just children
                         , attributes = Just (SolNode { _type = Just _type
                                                      , operator = Just operator
                                                      })
                         } =
  cat (punctuate (textStrict operator) (map pretty children)) </>
  textStrict ("@" <> _type)
prettyAssignment n = unexpectedPanic n

prettyIdentifier :: SolNode -> Doc
prettyIdentifier SolNode { name = Just "Identifier"
                         , children = Nothing
                         , attributes = Just (SolNode { _type = Just _type
                                                      , value = Just idName
                                                      })
                         } = textStrict (idName <> ":" <> _type)
prettyIdentifier n = unexpectedPanic n

prettyReturn :: SolNode -> Doc
prettyReturn SolNode {name = Just "Return", children = Just children} =
  textStrict "return" <> tupled (map pretty children)
prettyReturn n = unexpectedPanic n

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
