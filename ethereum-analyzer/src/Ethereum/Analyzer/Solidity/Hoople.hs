{-# LANGUAGE GADTs #-}

module Ethereum.Analyzer.Solidity.Hoople
  ( hoopleOf
  , HContract(..)
  , HFunDefinition(..)
  ) where

import Protolude hiding ((<*>), show)

import Compiler.Hoopl
import Ethereum.Analyzer.Solidity.Simple
import GHC.Show (Show(..))
import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import qualified Text.PrettyPrint.Leijen.Text as PP

data HContract = HContract
  { hcName :: Text
  , hcStateVars :: [VarDecl]
  , hcFunctions :: [HFunDefinition]
  }

type CFG = Graph HStatement O C

type ACFG = AGraph HStatement O C

data HFunDefinition = HFunDefinition
  { hfName :: Idfr
  , hfParams :: [VarDecl]
  , hfReturns :: [VarDecl]
  , hfCFG :: CFG
  }

instance Pretty Label where
  pretty = textStrict . toS . show

data HStatement e x where
  CoSt :: Label -> HStatement C O
  OoSt :: Statement -> HStatement O O
  OcSt :: Statement -> [Label] -> HStatement O C
  OcJump :: Label -> HStatement O C

instance Pretty (HStatement e x) where
  pretty (CoSt l) = pretty l
  pretty (OoSt st) = pretty st
  pretty (OcSt (StIf lval _ _) ll) =
    textStrict "OcIf" <+> pretty lval <+> textStrict "->" <+> prettyList ll
  pretty (OcSt (StLoop _) ll) =
    textStrict "OcLoop" <+> textStrict "->" <+> prettyList ll
  pretty (OcSt st ll) = pretty st <+> textStrict "->" <+> prettyList ll
  pretty (OcJump l) = textStrict "jump" <+> textStrict "->" <+> pretty l

instance Show (HStatement e x) where
  show = show . pretty

instance NonLocal HStatement where
  entryLabel (CoSt lbl) = lbl
  successors (OcSt _ lbls) = lbls
  successors (OcJump lbl) = [lbl]

instance HooplNode HStatement where
  mkBranchNode = OcJump
  mkLabelNode = CoSt

hoopleOf :: UniqueMonad m => Contract -> m HContract
hoopleOf Contract {cName = name, cStateVars = vars, cFunctions = funs} = do
  hFuns <- mapM hfunOf funs
  return $ HContract name vars hFuns

hfunOf :: UniqueMonad m => FunDefinition -> m HFunDefinition
hfunOf FunDefinition { fName = name
                     , fParams = params
                     , fReturns = returns
                     , fBody = stmts
                     } = do
  entryL <- freshLabel
  exitL <- freshLabel
  stmtGraph <- graphOf stmts exitL entryL
  let acfg = (mkBranch entryL |*><*| mkLabel entryL <*> stmtGraph) :: ACFG
  cfg <- graphOfAGraph acfg
  return $ HFunDefinition name params returns cfg

graphOf :: UniqueMonad m => [Statement] -> Label -> Label -> m ACFG
graphOf [] exitL _ = return $ mkBranch exitL
graphOf (h:t) exitL loopL = do
  postDom <- graphOf t exitL loopL
  case h of
    StLocalVarDecl _ -> return $ mkMiddle' h <*> postDom
    StAssign _ _ -> return $ mkMiddle' h <*> postDom
    StDelete _ -> return $ mkMiddle' h <*> postDom
    StTodo _ -> return $ mkMiddle' h <*> postDom
    StReturn _ -> do
      pdL <- freshLabel
      return $ mkLast (OcSt h []) |*><*| (mkLabel pdL <*> postDom)
    StThrow -> do
      pdL <- freshLabel
      return $ mkLast (OcSt h []) |*><*| (mkLabel pdL <*> postDom)
    StIf _ thenSts elseSts -> do
      pdL <- freshLabel
      thenG <- graphOf thenSts pdL loopL
      elseG <- graphOf elseSts pdL loopL
      -- TODO(zchn): remove the then and else statements in h when
      -- constructing OcSt.
      let ifte =
            mkIfThenElse
              (\thenLbl elseLbl -> mkLast (OcSt h [thenLbl, elseLbl]))
              thenG
              elseG
      return $ ifte |*><*| (mkLabel pdL <*> postDom)
    StLoop stmts -> do
      pdL <- freshLabel
      bodyLbl <- freshLabel
      bodyG <- graphOf stmts pdL bodyLbl
      return $
        mkBranch bodyLbl |*><*| (mkLabel bodyLbl <*> bodyG) |*><*|
        (mkLabel pdL <*> postDom)
    StBreak -> return $ mkBranch exitL
    StContinue -> return $ mkBranch loopL
  where
    mkMiddle' a = mkMiddle (OoSt a)
