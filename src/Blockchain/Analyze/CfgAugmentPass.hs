{-# LANGUAGE OverloadedStrings, FlexibleContexts,
  FlexibleInstances, GADTs, Rank2Types, DeriveGeneric, TypeFamilies, UndecidableInstances #-}

module Blockchain.Analyze.CfgAugmentPass
  ( doCfgAugmentPass
  ) where

import Blockchain.Analyze
import Blockchain.ExtWord
import Blockchain.VM.Opcodes
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
    coT _ f = f
    ooT :: HplOp O O -> StackTopFact -> StackTopFact
    ooT op _ = error ("Unimplemented(ooT):" ++ show op)
    ocT :: HplOp O C -> StackTopFact -> FactBase StackTopFact
    ocT op@(OcOp (_, JUMPI) _) f = distributeFact op Nothing
    ocT op _ = error ("Unimplemented(ocT): " ++ show op)
cfgAugmentRewrite :: FwdRewrite WordLabelMapFuelM HplOp StackTopFact
cfgAugmentRewrite = mkFRewrite3 coR ooR ocR
  where
    coR :: HplOp C O
        -> StackTopFact
        -> WordLabelMapFuelM (Maybe (Graph HplOp C O))
    coR = error "Unimplemented(coR)"
    ooR :: HplOp O O
        -> StackTopFact
        -> WordLabelMapFuelM (Maybe (Graph HplOp O O))
    ooR = error "Unimplemented(ooR)"
    ocR :: HplOp O C
        -> StackTopFact
        -> WordLabelMapFuelM (Maybe (Graph HplOp O C))
    ocR = error "Unimplemented(ocR)"

cfgAugmentPass :: FwdPass WordLabelMapFuelM HplOp (Maybe (Set Word256))
cfgAugmentPass =
  FwdPass
  { fp_lattice = stackTopLattice
  , fp_transfer = stackTopTransfer
  , fp_rewrite = noFwdRewrite -- cfgAugmentRewrite
  }

doCfgAugmentPass :: Label -> HplBody -> WordLabelMapM HplBody
doCfgAugmentPass entry body =
  runWithFuel
    1000000
    (fst <$> analyzeAndRewriteFwdBody cfgAugmentPass entry body noFacts)
