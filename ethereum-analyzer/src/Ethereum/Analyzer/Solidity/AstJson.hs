{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Ethereum.Analyzer.Solidity.AstJson
  ( SolNode(..)
  , decodeSoleNodes
  , defSolNode
  ) where

import Protolude hiding ((<>), show)

import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Lazy hiding (map)
import Ethereum.Analyzer.Common
import GHC.Show (Show(..))
import Text.PrettyPrint.Leijen.Text as PP

decodeSoleNodes :: LByteString -> Either Text [SolNode]
decodeSoleNodes combined_ast = do
  value <- s2t4Either (eitherDecode combined_ast) :: Either Text Value
  case value of
    Object o1 -> do
      srcObj <-
        maybeToRight "Could not find 'sources' in object" (lookup "sources" o1) :: Either Text Value
      case srcObj of
        Object o2 -> do
          let srcUnitObjs = elems o2
          mapM (s2t4Either . eitherDecode . encode) (srcUnitObjs :: [Value])
        _ -> Left "'sources''s value is not an Aeson Object"
    _ -> Left "input is not an Aeson Object"

-- import Data.Default
-- import GHC.Generics
data SolNode = SolNode
  { name :: Maybe Text
  , _id :: Maybe Int
  , _type :: Maybe Text
  , _AST :: Maybe SolNode
  , attributes :: Maybe SolNode
  , constant :: Maybe Bool
  , components :: Maybe [Maybe SolNode]
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
    genericToJSON
      defaultOptions
      {fieldLabelModifier = dropWhile (== '_'), omitNothingFields = True}

instance FromJSON SolNode where
  parseJSON =
    genericParseJSON
      defaultOptions
      {fieldLabelModifier = dropWhile (== '_'), omitNothingFields = True}

defSolNode :: SolNode
defSolNode =
  SolNode
  { children = Nothing
  , _id = Nothing
  , _type = Nothing
  , _AST = Nothing
  , attributes = Nothing
  , constant = Nothing
  , components = Nothing
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

instance Pretty SolNode where
  pretty n@SolNode {name = name}
    | isNothing name = prettyAst n -- Top level AST
    | name == Just "SourceUnit" = prettySourceUnit n
    | name == Just "PragmaDirective" = PP.empty
    | name == Just "ContractDefinition" = prettyContractDefinition n
    | name == Just "VariableDeclaration" = prettyVariableDeclaration n
    | name == Just "ElementaryTypeName" = prettyElementTypeName n
    | name == Just "FunctionDefinition" = prettyFunctionDefinition n
    | name == Just "ParameterList" = prettyParameterList n
    | name == Just "Block" = prettyBlock n
    | name == Just "ExpressionStatement" = prettyExpressionStatement n
    | name == Just "Assignment" = prettyAssignment n
    | name == Just "Identifier" = prettyIdentifier n
    | name == Just "Return" = prettyReturn n
    | name == Just "Mapping" = prettyMapping n
    | name == Just "IndexAccess" = prettyIndexAccess n
    | name == Just "MemberAccess" = prettyMemberAccess n
    | name == Just "IfStatement" = prettyIfStatement n
    | name == Just "BinaryOperation" = prettyBinaryOperation n
    | name == Just "VariableDeclarationStatement" =
      prettyVariableDeclarationStatement n
    | name == Just "FunctionCall" = prettyFunctionCall n
    | name == Just "UserDefinedTypeName" = prettyUserDefinedTypeName n
    | name == Just "Literal" = prettyLiteral n
    | otherwise = unimplementedPanic n

prettyAst :: SolNode -> Doc
prettyAst SolNode {name = Nothing, _AST = Just ast} = pretty ast
prettyAst n = unexpectedPanic n

prettySourceUnit :: SolNode -> Doc
prettySourceUnit SolNode {name = Just "SourceUnit", children = Just children} =
  textStrict "//--SourceUnit--" PP.<$> vsep (map pretty children)
prettySourceUnit n = unexpectedPanic n

prettyContractDefinition :: SolNode -> Doc
prettyContractDefinition SolNode { name = Just "ContractDefinition"
                                 , children = Just children
                                 , attributes = Just SolNode { children = Nothing
                                                             , name = Just cName
                                                             }
                                 } =
  textStrict "contract" </> textStrict cName </>
  semiBraces (map pretty children)
prettyContractDefinition n = unexpectedPanic n

prettyVariableDeclaration :: SolNode -> Doc
prettyVariableDeclaration SolNode { name = Just "VariableDeclaration"
                                  -- , children = Just children
                                  , children = Just _
                                  , attributes = Just SolNode { children = Nothing
                                                              , name = Just vName
                                                              , _type = Just vType
                                                              }
                                  }
  -- textStrict (vType <> "/") <> (tupled $ map pretty children) </> textStrict vName
 = textStrict vType </> textStrict vName
prettyVariableDeclaration n = unexpectedPanic n

prettyElementTypeName :: SolNode -> Doc
prettyElementTypeName SolNode { name = Just "ElementaryTypeName"
                              , children = Nothing
                              , attributes = Just SolNode {name = Just veType}
                              } = textStrict veType
prettyElementTypeName n = unexpectedPanic n

prettyFunctionDefinition :: SolNode -> Doc
prettyFunctionDefinition SolNode { name = Just "FunctionDefinition"
                                 , children = Just [params, returns, body]
                                 , attributes = Just SolNode { children = Nothing
                                                             , name = Just fName
                                                             }
                                 } =
  textStrict "fun" </>
  align (textStrict fName </> cat (map pretty [params, returns, body]))
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
                         , children = Just [lval, rval]
                         , attributes = Just SolNode { _type = Just _
                                                     , operator = Just operator
                                                     }
                         } = pretty lval <> textStrict operator <> pretty rval
prettyAssignment n = unexpectedPanic n

prettyIdentifier :: SolNode -> Doc
prettyIdentifier SolNode { name = Just "Identifier"
                         , children = Nothing
                         , attributes = Just SolNode { _type = Just _ -- _type
                                                     , value = Just idName
                                                     }
                         } -- textStrict (idName <> ":" <> _type)
 = textStrict idName
prettyIdentifier n = unexpectedPanic n

prettyReturn :: SolNode -> Doc
prettyReturn SolNode {name = Just "Return", children = Just children} =
  textStrict "return" <> tupled (map pretty children)
prettyReturn n = unexpectedPanic n

prettyMapping :: SolNode -> Doc
prettyMapping SolNode {name = Just "Mapping", children = Just children} =
  textStrict "mapping" <> tupled (map pretty children)
prettyMapping n = unexpectedPanic n

prettyIndexAccess :: SolNode -> Doc
prettyIndexAccess SolNode { name = Just "IndexAccess"
                          , children = Just (c1:ctail)
                          , attributes = Just SolNode {_type = Just _}
                          } = pretty c1 <> pretty ctail
prettyIndexAccess n = unexpectedPanic n

prettyMemberAccess :: SolNode -> Doc
prettyMemberAccess SolNode { name = Just "MemberAccess"
                           , children = Just [obj]
                           , attributes = Just SolNode { _type = Just _
                                                       , member_name = Just mName
                                                       }
                           } = pretty obj <> textStrict "." <> textStrict mName
prettyMemberAccess n = unexpectedPanic n

prettyIfStatement :: SolNode -> Doc
prettyIfStatement SolNode {name = Just "IfStatement", children = Just children} =
  textStrict "if" <> tupled (map pretty children)
prettyIfStatement n = unexpectedPanic n

prettyBinaryOperation :: SolNode -> Doc
prettyBinaryOperation SolNode { name = Just "BinaryOperation"
                              , children = Just [op1, op2]
                              , attributes = Just SolNode { _type = Just _
                                                          , operator = Just vOp
                                                          }
                              } =
  parens (pretty op1 <> textStrict vOp <> pretty op2)
prettyBinaryOperation n = unexpectedPanic n

prettyVariableDeclarationStatement :: SolNode -> Doc
prettyVariableDeclarationStatement SolNode { name = Just "VariableDeclarationStatement"
                                           , children = Just [vdec, vinit]
                                           } =
  pretty vdec </> textStrict "=" </> pretty vinit
prettyVariableDeclarationStatement n = unexpectedPanic n

prettyFunctionCall :: SolNode -> Doc
prettyFunctionCall SolNode { name = Just "FunctionCall"
                           , children = Just (func:params)
                           , attributes = Just SolNode {_type = Just _}
                           } = pretty func <> tupled (map pretty params)
prettyFunctionCall n = unexpectedPanic n

prettyUserDefinedTypeName :: SolNode -> Doc
prettyUserDefinedTypeName SolNode { name = Just "UserDefinedTypeName"
                                  , children = Nothing
                                  , attributes = Just SolNode {name = Just vName}
                                  } = textStrict vName
prettyUserDefinedTypeName n = unexpectedPanic n

prettyLiteral :: SolNode -> Doc
prettyLiteral SolNode { name = Just "Literal"
                      , children = Nothing
                      , attributes = Just SolNode { _type = Just _
                                                  , value = Just vValue
                                                  }
                      } = textStrict vValue
prettyLiteral n = unexpectedPanic n
