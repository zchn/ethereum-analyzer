module Blockchain.Analyze.CfgAugmentPass
  ( doCfgAugmentPass
  ) where

import Blockchain.Analyze
import Blockchain.ExtWord
import Compiler.Hoopl
import Data.Maybe
import Data.Set

-- TODO(zchn): Use WithTop and liftJoinTop instead
type StackTopFact = Maybe (Set Word256)

stackTopLattice :: DataflowLattice (Maybe (Set Word256))
stackTopLattice =
  DataflowLattice
  { fact_name = "stackTopLattice"
  , fact_bot = Just empty
  , fact_join = joinStackTopLattice
  }

joinStackTopLattice
  :: Label
  -> OldFact StackTopFact
  -> NewFact StackTopFact
  -> (ChangeFlag, StackTopFact)
joinStackTopLattice _ (OldFact Nothing) (NewFact _) = (NoChange, Nothing)
joinStackTopLattice _ (OldFact _) (NewFact Nothing) = (SomeChange, Nothing)
joinStackTopLattice _ (OldFact oldF) (NewFact newF) =
  let joinedF = Just $ unions $ catMaybes [oldF, newF]
  in (changeIf $ joinedF /= oldF, joinedF)

stackTopTransfer :: FwdTransfer HplOp StackTopFact
stackTopTransfer = mkFTransfer3 coT ooT ocT
  where
    coT :: HplOp C O -> StackTopFact -> StackTopFact
    coT = error "Unimplemented"
    ooT :: HplOp O O -> StackTopFact -> StackTopFact
    ooT = error "Unimplemented"
    ocT :: HplOp O C -> StackTopFact -> FactBase StackTopFact
    ocT = error "Unimplemented"

cfgAugmentRewrite :: FwdRewrite (CheckingFuelMonad SimpleUniqueMonad) HplOp StackTopFact
cfgAugmentRewrite = mkFRewrite3 coR ooR ocR
  where
    coR :: HplOp C O
        -> StackTopFact
        -> SimpleFuelMonad (Maybe (Graph HplOp C O))
    coR = error "Unimplemented"
    ooR :: HplOp O O
        -> StackTopFact
        -> SimpleFuelMonad (Maybe (Graph HplOp O O))
    ooR = error "Unimplemented"
    ocR :: HplOp O C
        -> StackTopFact
        -> SimpleFuelMonad (Maybe (Graph HplOp O C))
    ocR = error "Unimplemented"

cfgAugmentPass :: FwdPass (CheckingFuelMonad SimpleUniqueMonad) HplOp (Maybe (Set Word256))
cfgAugmentPass =
  FwdPass
  { fp_lattice = stackTopLattice
  , fp_transfer = stackTopTransfer
  , fp_rewrite = cfgAugmentRewrite
  }

doCfgAugmentPass :: Label -> HplBody -> SimpleUniqueMonad HplBody
doCfgAugmentPass entry body =
  runWithFuel
    1000000
    (fst <$> analyzeAndRewriteFwdBody cfgAugmentPass entry body noFacts)
