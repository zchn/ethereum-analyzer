{-# LANGUAGE OverloadedStrings, FlexibleContexts, FlexibleInstances, GADTs, Rank2Types #-}

module Blockchain.Analyze.IR (ExtOp(..), e2h, h2e, oo2e, oc2e) where

import Blockchain.ExtWord as BE
import Blockchain.VM.Opcodes as BVO
import Compiler.Hoopl as CH


data ExtOp = EvmOp { offset :: Word256, op :: Operation }
  | ExtOp deriving (Eq)

data HplOp e x where
  CoOp :: Label -> HplOp CH.C CH.O
  OoOp :: ExtOp -> HplOp CH.O CH.O
  OcOp :: ExtOp -> [Label] -> HplOp CH.O CH.C

instance Show (HplOp e x)

instance Eq (HplOp CH.C CH.O) where
  (==) (CoOp a) (CoOp b) = a == b

instance Eq (HplOp CH.O CH.O) where
  (==) (OoOp a) (OoOp b) = a == b

instance Eq (HplOp CH.O CH.C) where
  (==) (OcOp a _) (OcOp b _) = a == b

instance NonLocal HplOp where
  entryLabel (CoOp l) = l
  successors (OcOp _ ll) = ll

e2h :: ExtOp -> Either (HplOp CH.O CH.O) (HplOp CH.O CH.C)
e2h a@(EvmOp _ JUMPDEST) = Left $ OoOp a
e2h a@(EvmOp _ POP) = Left $ OoOp a
e2h _ = error "Unimplemented!"

h2e :: Either (HplOp CH.O CH.O) (HplOp CH.O CH.C) -> ExtOp
h2e _ = error "Unimplemented!"

oo2e :: HplOp CH.O CH.O -> ExtOp
oo2e _ = error "Unimplemented!"

oc2e :: HplOp CH.O CH.C -> ExtOp
oc2e _ = error "Unimplemented!"
