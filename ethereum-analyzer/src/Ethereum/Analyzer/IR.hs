{-# LANGUAGE OverloadedStrings, FlexibleContexts,
  FlexibleInstances, GADTs, Rank2Types, TypeFamilies,
  UndecidableInstances #-}

module Ethereum.Analyzer.IR
  ( HplBody
  , HplCode(..)
  , HplContract(..)
  , HplOp(..)
  , WordLabelMapM
  , WordLabelMapFuelM
  , unWordLabelMapM
  , evmOps2HplCode
  , evmOps2HplContract
  , labelFor
  , labelsFor
  , showOp
  , showOps
  ) where

import Blockchain.ExtWord as BE
import Blockchain.VM.Opcodes as BVO
import Compiler.Hoopl as CH
import Control.Monad as CM
import Data.Bimap as DB

import Data.List as DL

-- import Data.Graph.Inductive.Graph as DGIG
import Data.Text as DT
import qualified Data.Text.Lazy as DTL
import Legacy.Haskoin.V0102.Network.Haskoin.Crypto.BigWord

data HplOp e x where
        CoOp :: Label -> HplOp C O
        OoOp :: (Word256, Operation) -> HplOp O O
        OcOp :: (Word256, Operation) -> [Label] -> HplOp O C
        HpCodeCopy :: Word256 -> HplOp O O

showLoc :: Word256 -> String
showLoc = show . getBigWordInteger

showOp :: (Word256, Operation) -> String
showOp (lineNo, op) = showLoc lineNo ++ ": " ++ show op

showOps :: [(Word256, Operation)] -> [String]
showOps = Prelude.map showOp

instance Show (HplOp e x) where
  show (CoOp l) = "CO: " ++ show l
  show (OoOp op) = "OO: " ++ showOp op
  show (OcOp op ll) = "OC: " ++ showOp op ++ " -> " ++ show ll
  show (HpCodeCopy offset) = "HpCodeCopy " ++ show offset

instance Show (Block HplOp C C) where
  show a =
    let (h, m, t) = blockSplit a
    in DL.unlines $ [show h] ++ DL.map show (blockToList m) ++ [show t]

instance Eq (HplOp C O) where
  (==) (CoOp a) (CoOp b) = a == b

instance Eq (HplOp O O) where
  (==) (OoOp a) (OoOp b) = a == b
  (==) (HpCodeCopy a) (HpCodeCopy b) = a == b
  (==) _ _ = False

instance Eq (HplOp O C) where
  (==) (OcOp a _) (OcOp b _) = a == b

instance NonLocal HplOp where
  entryLabel (CoOp l) = l
  successors (OcOp _ ll) = ll

type HplBody = Body HplOp

data HplCode = HplCode
  { entryOf :: Maybe Label
  , bodyOf :: HplBody
  } deriving (Show)

data HplContract = HplContract
  { ctorOf :: HplCode
  , dispatcherOf :: HplCode
  } deriving (Show)

emptyCode :: HplCode
emptyCode = HplCode Nothing emptyBody

evmOps2HplContract :: [(Word256, Operation)] -> WordLabelMapM HplContract
evmOps2HplContract l = do
  ctorBody <- evmOps2HplCode l
  return HplContract {ctorOf = ctorBody, dispatcherOf = emptyCode}

evmOps2HplCode :: [(Word256, Operation)] -> WordLabelMapM HplCode
evmOps2HplCode [] = return emptyCode
evmOps2HplCode l@((loc, _):_) = do
  entry <- labelFor loc
  body <- _evmOps2HplBody l
  return HplCode {entryOf = Just entry, bodyOf = body}

_evmOps2HplBody :: [(Word256, Operation)] -> WordLabelMapM HplBody
_evmOps2HplBody [] = return emptyBody
_evmOps2HplBody el@((loc, _):_) = do
  l <- labelFor loc
  doEvmOps2HplBody emptyBody (blockJoinHead (CoOp l) emptyBlock) el
  where
    doEvmOps2HplBody
      :: HplBody
      -> Block HplOp C O
      -> [(Word256, Operation)]
      -> WordLabelMapM HplBody
    doEvmOps2HplBody body _ [] = return body -- sliently discarding bad hds
    doEvmOps2HplBody body hd [h'] =
      if isTerminator (snd h')
        then return $ addBlock (blockJoinTail hd (OcOp h' [])) body
        else return body
    doEvmOps2HplBody body hd (h':(t'@((loc', op'):_)))
      | isTerminator (snd h') = do
        l' <- labelFor loc'
        doEvmOps2HplBody
          (addBlock
             (blockJoinTail hd (OcOp h' [l' | canPassThrough (snd h')]))
             body)
          (blockJoinHead (CoOp l') emptyBlock)
          t'
      | op' /= JUMPDEST = doEvmOps2HplBody body (blockSnoc hd (OoOp h')) t'
      | otherwise = do
        l' <- labelFor loc'
        doEvmOps2HplBody
          (addBlock
             (blockJoinTail hd (OcOp h' [l' | canPassThrough (snd h')]))
             body)
          (blockJoinHead (CoOp l') emptyBlock)
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

instance UnWordLabelMapM String where
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
