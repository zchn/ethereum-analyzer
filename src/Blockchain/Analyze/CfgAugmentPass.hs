module Blockchain.Analyze.CfgAugmentPass
  ( doCfgAugmentPass
  ) where

import Blockchain.Analyze
import Blockchain.ExtWord
import Compiler.Hoopl
import Data.Maybe
import Data.Set

doCfgAugmentPass :: HplBody -> WordLabelMapM HplBody
doCfgAugmentPass = error "Unimplemented(doCfgAugmentPass)."

type StackTopFact = Maybe (Set Word256)

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
