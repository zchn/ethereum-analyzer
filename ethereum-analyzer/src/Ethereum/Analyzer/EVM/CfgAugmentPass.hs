{-# LANGUAGE OverloadedStrings, FlexibleContexts,
  FlexibleInstances, GADTs, Rank2Types, TypeFamilies,
  UndecidableInstances #-}

module Ethereum.Analyzer.EVM.CfgAugmentPass
  ( doCfgAugmentPass
  ) where

import Protolude hiding (show)

import Blockchain.ExtWord
import Blockchain.VM.Opcodes
import Compiler.Hoopl

-- import Data.Bits
import Data.List as DL
import Data.Set as DS hiding (toList)
import Ethereum.Analyzer.Common
import Ethereum.Analyzer.EVM.IR
import GHC.Show

type StackTopFact = WithTop (Set Word256)

joinJumpTargets
  :: Label
  -> OldFact (Set Word256)
  -> NewFact (Set Word256)
  -> (ChangeFlag, Set Word256)
joinJumpTargets _ (OldFact oldF) (NewFact newF) =
  if newF `isSubsetOf` oldF
    then (NoChange, oldF)
    else (SomeChange, oldF `DS.union` newF)

joinStackTopFact
  :: Label
  -> OldFact StackTopFact
  -> NewFact StackTopFact
  -> (ChangeFlag, StackTopFact)
joinStackTopFact = liftJoinTop joinJumpTargets

stackTopLattice :: DataflowLattice StackTopFact
stackTopLattice =
  DataflowLattice
  { fact_name = "stackTopLattice"
  , fact_bot = PElem DS.empty
  , fact_join = joinStackTopFact
  }

stackTopTransfer :: FwdTransfer HplOp StackTopFact
stackTopTransfer = mkFTransfer3 coT ooT ocT
  where
    coT :: HplOp C O -> StackTopFact -> StackTopFact
    coT _ f = f
    ooT :: HplOp O O -> StackTopFact -> StackTopFact
    ooT (OoOp (_, op)) f = opT op f
    ooT (HpCodeCopy _) f = f
    ocT :: HplOp O C -> StackTopFact -> FactBase StackTopFact
    ocT hplop@(OcOp (_, op) _) f = distributeFact hplop (opT op f)
    ocT hplop@(HpJump _ _) f = distributeFact hplop f
    ocT hplop@(HpEnd _) f = distributeFact hplop f
    opT :: Operation -> StackTopFact -> StackTopFact
    opT DUP1 f = f
    opT ISZERO (PElem st) =
      PElem $
      DS.map
        (\wd ->
           if wd == 0
             then 1
             else 0)
        st
    opT JUMPDEST f = f
    opT NEG (PElem st) = PElem $ DS.map (\wd -> -wd) st
    opT NOT (PElem st) =
      PElem $ DS.map (bytesToWord256 . DL.map complement . word256ToBytes) st
    opT (PUSH w8l) _ = PElem $ DS.singleton $ varBytesToWord256 w8l
    opT op@LABEL {} _ =
      panic $ "Unexpected(stackTopTransfer): " <> toS (show op)
    opT op@PUSHLABEL {} _ =
      panic $ "Unexpected(stackTopTransfer): " <> toS (show op)
    opT op@PUSHDIFF {} _ =
      panic $ "Unexpected(stackTopTransfer): " <> toS (show op)
    opT op@DATA {} _ = panic $ "Unexpected(stackTopTransfer): " <> toS (show op)
    opT op@MalformedOpcode {} _ =
      panic $ "Unexpected(stackTopTransfer): " <> toS (show op)
    opT _ _ = Top

opGUnit :: HplOp e x -> Graph HplOp e x
opGUnit co@CoOp {} = gUnitCO $ BlockCO co BNil
opGUnit oo@OoOp {} = gUnitOO $ BMiddle oo
opGUnit oo@HpCodeCopy {} = gUnitOO $ BMiddle oo
opGUnit oc@OcOp {} = gUnitOC $ BlockOC BNil oc
opGUnit oc@HpJump {} = gUnitOC $ BlockOC BNil oc
opGUnit oc@HpEnd {} = gUnitOC $ BlockOC BNil oc

cfgAugmentRewrite :: FwdRewrite WordLabelMapFuelM HplOp StackTopFact
cfgAugmentRewrite = mkFRewrite3 coR ooR ocR
  where
    coR :: HplOp C O
        -> StackTopFact
        -> WordLabelMapFuelM (Maybe (Graph HplOp C O))
    coR op _ = return $ Just $ opGUnit op
    ooR :: HplOp O O
        -> StackTopFact
        -> WordLabelMapFuelM (Maybe (Graph HplOp O O))
    ooR op _ = return $ Just $ opGUnit op
    ocR :: HplOp O C
        -> StackTopFact
        -> WordLabelMapFuelM (Maybe (Graph HplOp O C))
    ocR op@HpJump {} _ = return (Just (opGUnit op))
    ocR op@HpEnd {} _ = return (Just (opGUnit op))
    ocR op@(OcOp (loc, ope) ll) f =
      case ope of
        JUMP -> handleJmp
        JUMPI -> handleJmp
        _ -> return $ Just $ opGUnit op
      where
        handleJmp :: WordLabelMapFuelM (Maybe (Graph HplOp O C))
        handleJmp =
          case f of
            Top -> return $ Just $ opGUnit op -- TODO(zchn): Should return all targets
            PElem st -> do
              newll <- liftFuel $ labelsFor $ toList st
              return $
                Just $
                opGUnit $ OcOp (loc, ope) $ toList $ fromList (ll <> newll)

cfgAugmentPass :: FwdPass WordLabelMapFuelM HplOp StackTopFact
cfgAugmentPass =
  FwdPass
  { fp_lattice = stackTopLattice
  , fp_transfer = stackTopTransfer
  , fp_rewrite = cfgAugmentRewrite
  }

doCfgAugmentPass :: HplContract -> WordLabelMapM HplContract
doCfgAugmentPass contract = do
  let body = bodyOf $ ctorOf contract
  newBody <- runWithFuel 1000000
               ((\(a,_,_) -> a) <$>
                 analyzeAndRewriteFwdOx
                 cfgAugmentPass
                 body
                 Top)
  return contract {ctorOf = HplCode newBody}
