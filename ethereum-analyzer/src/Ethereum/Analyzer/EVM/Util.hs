{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs,
  NoImplicitPrelude, Rank2Types, TypeFamilies, UndecidableInstances
  #-}

module Ethereum.Analyzer.EVM.Util
  ( toDotText
  , disasmToDotText
  , disasmToDotText2
  ) where

import Protolude hiding (show)

import Compiler.Hoopl
import Data.Graph.Inductive.Graph as DGIG
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz
import Data.GraphViz.Printing hiding ((<>))
import qualified Data.Text.Lazy as DTL
import Ethereum.Analyzer.EVM.CfgAugWithTopNPass
import Ethereum.Analyzer.EVM.CfgAugmentPass
import Ethereum.Analyzer.EVM.Disasm
import Ethereum.Analyzer.EVM.IR
import GHC.Show
import Text.Read (read)

disasmToDotText
  :: HasEvmBytecode a
  => a -> Text
disasmToDotText a =
  let disasmd = disasm a
      result =
        unWordLabelMapM $ do
          contract <- evmOps2HplContract disasmd
          toDotText <$> (bodyOf . ctorOf <$> doCfgAugmentPass contract)
  in result

disasmToDotText2
  :: HasEvmBytecode a
  => a -> (Text, Text)
disasmToDotText2 a =
  let result =
        unWordLabelMapM $ do
          contract' <- doCfgAugWithTopNPass a
          return
            ( toDotText $ bodyOf (ctorOf contract')
            , toDotText $ bodyOf (dispatcherOf contract'))
  in result

toDotText :: HplBody -> Text
toDotText bd =
  let bdGr = toGr bd
      dotG = toDotGraph bdGr
      dotCode = toDot dotG
  in DTL.toStrict $ renderDot dotCode

toGr :: HplBody -> Gr (Block HplOp C C) ()
toGr bd =
  let lblToNode l = read (drop 1 $ toS $ show l)
      (nList, eList) =
        mapFoldWithKey
          (\lbl blk (nList', eList') ->
             let node = lblToNode lbl
                 edgs = map (\l -> (node, lblToNode l, ())) (successors blk)
             in (nList' <> [(node, blk)], eList' <> edgs))
          ([], [])
          bd
  in mkGraph nList eList

visParams
  :: forall n el.
     GraphvizParams n (Block HplOp C C) el () (Block HplOp C C)
visParams =
  nonClusteredParams
  {fmtNode = \(_, nl) -> [textLabel (toS $ show nl), shape BoxShape]}

toDotGraph :: Gr (Block HplOp C C) () -> DotGraph Node
toDotGraph = graphToDot visParams
