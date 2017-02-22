{-# LANGUAGE OverloadedStrings, FlexibleContexts,
  FlexibleInstances, GADTs, Rank2Types, DeriveGeneric #-}

module Blockchain.Analyze.IR
  ( HplOp
  , evmOps2HplBody
  , hplBody2EvmOps
  , runWordLabelMapM
  ) where

import Blockchain.ExtWord as BE
import Blockchain.VM.Opcodes as BVO
import Compiler.Hoopl as CH
import Control.Monad as CM
import Data.Bimap as DB

data HplOp e x where
        CoOp :: Word256 -> Label -> HplOp CH.C CH.O
        OoOp :: (Word256, Operation) -> HplOp CH.O CH.O
        OcOp :: (Word256, Operation) -> [Label] -> HplOp CH.O CH.C

instance Show (HplOp e x)

instance Eq (HplOp CH.C CH.O) where
  (==) (CoOp a _) (CoOp b _) = a == b

instance Eq (HplOp CH.O CH.O) where
  (==) (OoOp a) (OoOp b) = a == b

instance Eq (HplOp CH.O CH.C) where
  (==) (OcOp a _) (OcOp b _) = a == b

instance NonLocal HplOp where
  entryLabel (CoOp _ l) = l
  successors (OcOp _ ll) = ll

evmOps2HplBody :: [(Word256, Operation)] -> WordLabelMapM (Body HplOp)
evmOps2HplBody el = error "Unimplemented"

hplBody2EvmOps :: (Body HplOp) -> [(Word256, Operation)]
hplBody2EvmOps b = error "Unimplemented"

runWordLabelMapM :: WordLabelMapM [(Word256, Operation)]
                 -> [(Word256, Operation)]
runWordLabelMapM (WordLabelMapM fun) = snd $ runSimpleUniqueMonad $ fun empty

--------------------------------------------------------------------------------
-- The WordLabelMapM monad
--------------------------------------------------------------------------------
type WordLabelMap = Bimap Word256 Label

data WordLabelMapM a =
  WordLabelMapM (WordLabelMap -> SimpleUniqueMonad (WordLabelMap, a))

labelFor :: Word256 -> WordLabelMapM Label
labelFor word = WordLabelMapM f
  where
    f m =
      case DB.lookup word m of
        Just l' -> return (m, l')
        Nothing -> do
          l' <- CH.freshLabel
          let m' = DB.insert word l' m
          return (m', l')

labelsFor :: [Word256] -> WordLabelMapM [Label]
labelsFor = mapM labelFor

instance Monad WordLabelMapM where
  return = pure
  WordLabelMapM f1 >>= k =
    WordLabelMapM $
    \m -> do
      (m', x) <- f1 m
      let (WordLabelMapM f2) = k x
      f2 m'

instance Functor WordLabelMapM where
  fmap = liftM

instance Applicative WordLabelMapM where
  pure x = WordLabelMapM (\m -> return (m, x))
  (<*>) = ap
