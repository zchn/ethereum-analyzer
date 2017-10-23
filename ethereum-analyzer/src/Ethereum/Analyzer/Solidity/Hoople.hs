module Ethereum.Analyzer.Solidity.Hoople
    ( hoopleOf
    ) where

import Compiler.Hoople
import Ethereum.Analyzer.Solidity.Simple

data HContract = HContract
  { hcName :: Text
  , hcStateVars :: [VarDecl]
  , hcFunctions :: [HFunDefinition]
  } deriving (Eq, Generic, Show, GP.Out)


type CFG = Graph HStatement O C

data HFunDefinition = HFunDefinition
  { hfName :: Idfr
  , hfParams :: [VarDecl]
  , hfReturns :: [VarDecl]
  , hfCFG :: CFG
  } deriving (Eq, Generic, Show, GP.Out)

data HStatement e x where
  CoSt :: Label -> HStatement C O
  OoSt :: Statement -> HStatement O O
  OcSt :: Statement -> [Label] -> HStatement O C

hoopleOf :: UniqueMonad m => Contract -> m HContract
hoopleOf Contract { cName = name
                  , cStateVars = vars
                  , cFunctions = funs
                  } = do
  hFuns <- mapM hfunOf funs
  return $ HContract name vars hFuns

hfunOf :: UniqueMonad m => FunDefinition -> m HFunDefinition
hfunOf FunDefinition { fName = name
                     , fParams = params
                     , fReturns = returns
                     , fBody = stmts } = do
  entryL <- freshLabel
  exitL <- freshLabel
  cfg <- mkBranch entryL <*> mkLabel entryL <*> graphOf stmts exitL entryL
  return $ HFunDefinition name params returns lbl cfg

graphOf :: UniqueMonad m => [Statement] -> Label -> Label -> m CFG
graphOf [] exitL _ = return $ mkBranch exitL
graphOf h:t exitL loopL = do
  postDom <- graphOf t exitL loopL
  case h of
  | StLocalVarDecl _ ->  return $ mkMiddle' h <*> postDom
  | StAssign _ _ -> return $ mkMiddle' h <*> postDom
  | StDelete _ -> return $ mkMiddle' h <*> postDom
  | StTodo _ -> return $ mkMiddle' h <*> postDom
  | StReturn _ -> do
      pdL <- freshLabel
      return $ mkLast (OcSt h []) |*><*| (mkLabel pdL <*> postDom)
  | StThrow -> do
      pdL <- freshLabel
      return $ mkLast (OcSt h []) |*><*| (mkLabel pdL <*> postDom)
  | StIf _ thenSts elseSts -> do
      pdL <- freshLabel
      thenG <- graphOf thenSts pdL loopL
      elseG <- graphOf elseSts pdL loopL
      -- TODO(zchn): remove the then and else statements in h when
      -- constructing OcSt.
      ifte = mkIfthenElse (\thenLbl elseLbl -> mkLast (OcSt h [thenLbl, elseLbl])) thenG elseG
      return $ ifte |*><*| (mkLabel pdL <*> postDom)
  | StLoop stmts -> do
      pdL <- freshLabel
      bodyLbl <- freshLabel
      bodyG <- graphOf stmts pdL bodyLbl
      return $ mkBranch bodyLbl |*><*| (mkLabel bodyLbl <*> bodyG) |*><*| (mkLabel pdL <*> postDom)
  | StBreak -> return $ mkBranch exitL
  | StContinue -> return $ mkBranch loopL
  where mkMiddle' a = mkMiddle (OoSt a)
