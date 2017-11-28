{-# LANGUAGE FlexibleContexts, OverloadedStrings,
  NoImplicitPrelude, FlexibleInstances, GADTs, Rank2Types,
  TypeFamilies, ScopedTypeVariables, UndecidableInstances #-}

module Ethereum.Analyzer.EVM.IR
  ( HplCfg
  , HplContract(..)
  , HplOp(..)
  , WordLabelMapM
  , WordLabelMapFuelM
  , unWordLabelMapM
  , evmOps2HplCfg
  , evmOps2HplContract
  , labelFor
  , labelsFor
  , showOp
  , showOps
  ) where

import Protolude hiding (show)

import Blockchain.ExtWord as BE
import Blockchain.VM.Opcodes as BVO
import Compiler.Hoopl as CH

-- import Control.Monad as CM
import Data.Bimap as DB
import Data.List as DL
import Data.String (String)

-- import Data.Graph.Inductive.Graph as DGIG
import qualified Data.Text.Lazy as DTL
import GHC.Show
import Legacy.Haskoin.V0102.Network.Haskoin.Crypto.BigWord

newtype MyLabel = MyLabel
  { unMyLabel :: Label
  } deriving (Eq)

data HplOp e x where
        CoOp :: Label -> HplOp C O
        OoOp :: (Word256, Operation) -> HplOp O O
        OcOp :: (Word256, Operation) -> [Label] -> HplOp O C
        HpJump :: MyLabel -> Label -> HplOp O C
        HpEnd :: MyLabel -> HplOp O C
        HpCodeCopy :: Word256 -> HplOp O O

showLoc :: Word256 -> String
showLoc = show . getBigWordInteger

showOp :: (Word256, Operation) -> String
showOp (lineNo, op) = showLoc lineNo <> ": " <> show op

showOps :: [(Word256, Operation)] -> [String]
showOps = fmap showOp

instance Show (HplOp e x) where
  show (CoOp l) = "CO: " <> show l
  show (OoOp op) = "OO: " <> showOp op
  show (OcOp op ll) = "OC: " <> showOp op <> " -> " <> show ll
  show (HpJump _ l) = "OC: HpJump -> " <> show l
  show HpEnd {} = "OC: HpEnd"
  show (HpCodeCopy offset) = "HpCodeCopy " <> show offset

instance Eq (HplOp C O) where
  (==) (CoOp a) (CoOp b) = a == b

instance Eq (HplOp O O) where
  (==) (OoOp a) (OoOp b) = a == b
  (==) (HpCodeCopy a) (HpCodeCopy b) = a == b
  (==) _ _ = False

instance Eq (HplOp O C) where
  (==) (OcOp a _) (OcOp b _) = a == b
  (==) (HpJump l1 _) (HpJump l2 _) = l1 == l2
  (==) (HpEnd l1) (HpEnd l2) = l1 == l2
  (==) _ _ = False

instance NonLocal HplOp where
  entryLabel (CoOp l) = l
  successors (OcOp _ ll) = ll
  successors (HpJump _ ll) = [ll]
  successors (HpEnd _) = []

type HplCfg = Graph HplOp O C

instance Show HplCfg where
  show = showGraph show

data HplContract = HplContract
  { ctorOf :: HplCfg
  , dispatcherOf :: HplCfg
  } deriving (Show)

emptyHplCfg
  :: UniqueMonad m
  => m HplCfg
emptyHplCfg = do
  l <- myFreshLabel
  return $ mkLast (HpEnd l)

evmOps2HplContract :: [(Word256, Operation)] -> WordLabelMapM HplContract
evmOps2HplContract l = do
  ctorBody <- evmOps2HplCfg l
  ec <- emptyHplCfg
  return HplContract {ctorOf = ctorBody, dispatcherOf = ec}

myFreshLabel
  :: UniqueMonad m
  => m MyLabel
myFreshLabel = fmap MyLabel freshLabel

evmOps2HplCfg :: [(Word256, Operation)] -> WordLabelMapM HplCfg
evmOps2HplCfg [] = emptyHplCfg
evmOps2HplCfg el@((loc, _):_) = do
  l <- labelFor loc
  jpLabel <- myFreshLabel
  doEvmOps2HplCfg (mkLast $ HpJump jpLabel l) (mkFirst $ CoOp l) el
  where
    doEvmOps2HplCfg :: HplCfg
                    -> Graph HplOp C O
                    -> [(Word256, Operation)]
                    -> WordLabelMapM HplCfg
    doEvmOps2HplCfg body _ [] = return body -- sliently discarding bad hds
    doEvmOps2HplCfg body hd [h'] =
      if isTerminator (snd h')
        then return $ body |*><*| hd CH.<*> mkLast (OcOp h' [])
        else return body -- sliently discarding bad hds
    doEvmOps2HplCfg body hd (h':(t'@((loc', op'):_)))
      | isTerminator (snd h') = do
        l' <- labelFor loc'
        doEvmOps2HplCfg
          (body |*><*| hd CH.<*> mkLast (OcOp h' [l' | canPassThrough (snd h')]))
          (mkFirst $ CoOp l')
          t'
      | op' /= JUMPDEST = doEvmOps2HplCfg body (hd CH.<*> mkMiddle (OoOp h')) t'
      | otherwise = do
        l' <- labelFor loc'
        doEvmOps2HplCfg
          (body |*><*| hd CH.<*> mkLast (OcOp h' [l' | canPassThrough (snd h')]))
          (mkFirst $ CoOp l')
          t'

isTerminator :: Operation -> Bool
isTerminator STOP = True
isTerminator JUMP = True
isTerminator JUMPI = True
isTerminator CALL = True
isTerminator CALLCODE = True
isTerminator RETURN = True
isTerminator DELEGATECALL = True
isTerminator INVALID = True
isTerminator SUICIDE = True
isTerminator _ = False

canPassThrough :: Operation -> Bool
canPassThrough STOP = False
canPassThrough JUMP = False
canPassThrough RETURN = False
canPassThrough INVALID = False
canPassThrough SUICIDE = False
canPassThrough _ = True

--------------------------------------------------------------------------------
-- The WordLabelMapM monad
--------------------------------------------------------------------------------
type WordLabelMap = Bimap Word256 Label

newtype WordLabelMapM a =
  WordLabelMapM (WordLabelMap -> SimpleUniqueMonad (WordLabelMap, a))

instance CheckpointMonad WordLabelMapM where
  type Checkpoint WordLabelMapM = (WordLabelMap, Checkpoint SimpleUniqueMonad)
  checkpoint =
    let mapper
          :: WordLabelMap
          -> SimpleUniqueMonad (WordLabelMap, Checkpoint WordLabelMapM)
        mapper m = do
          suCheckpoint <- CH.checkpoint
          return (m, (m, suCheckpoint))
    in WordLabelMapM mapper
  restart (m, suCheckpoint) =
    let mapper :: WordLabelMap -> CH.SimpleUniqueMonad (WordLabelMap, ())
        mapper _ = do
          _ <- CH.restart suCheckpoint
          return (m, ())
    in WordLabelMapM mapper

instance UniqueMonad WordLabelMapM where
  freshUnique = WordLabelMapM f
    where
      f m = do
        u <- freshUnique
        return (m, u)

type WordLabelMapFuelM = CheckingFuelMonad WordLabelMapM

labelFor :: Word256 -> WordLabelMapM Label
labelFor word = WordLabelMapM f
  where
    f m =
      case DB.lookup word m of
        Just l' -> return (m, l')
        Nothing -> do
          l' <- freshLabel
          let m' = DB.insert word l' m
          return (m', l')

labelsFor :: [Word256] -> WordLabelMapM [Label]
labelsFor = mapM labelFor

instance Monad WordLabelMapM where
  return = pure
  WordLabelMapM f1 >>= k =
    WordLabelMapM $ \m -> do
      (m', x) <- f1 m
      let (WordLabelMapM f2) = k x
      f2 m'

instance Functor WordLabelMapM where
  fmap = liftM

instance Applicative WordLabelMapM where
  pure x = WordLabelMapM (\m -> return (m, x))
  (<*>) = ap

class UnWordLabelMapM a where
  unWordLabelMapM :: WordLabelMapM a -> a

instance UnWordLabelMapM Int where
  unWordLabelMapM = internalUnWordLabelMapM

instance UnWordLabelMapM Text where
  unWordLabelMapM = internalUnWordLabelMapM

instance UnWordLabelMapM DTL.Text where
  unWordLabelMapM = internalUnWordLabelMapM

instance (UnWordLabelMapM a, UnWordLabelMapM b) =>
         UnWordLabelMapM (a, b) where
  unWordLabelMapM = internalUnWordLabelMapM

internalUnWordLabelMapM :: WordLabelMapM a -> a
internalUnWordLabelMapM (WordLabelMapM f) =
  snd $ runSimpleUniqueMonad (f DB.empty)
