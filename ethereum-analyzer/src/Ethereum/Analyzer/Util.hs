{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs,
  NoImplicitPrelude, Rank2Types, TypeFamilies, UndecidableInstances
  #-}

module Ethereum.Analyzer.Util
  ( toDotText
  ) where

import Protolude hiding (show)

import Compiler.Hoopl as CH
import Ckev.In.Text
import Data.Graph.Inductive.Graph as DGIG
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz
import Data.GraphViz.Printing hiding ((<>))
import qualified Data.Text as DT
import qualified Data.Text.Lazy as DTL
import Text.Read (read)

newtype PBlock n = PBlock (Block n C C)

instance (ShowText (n C O), ShowText (n O O), ShowText (n O C)) => ShowText (PBlock n) where
  showText (PBlock a) =
    let (h, m, t) = blockSplit a
    in DT.unlines $ [showText h] <> map showText (blockToList m) <> [showText t]

toDotText :: (NonLocal n, ShowText (PBlock n)) => CH.Graph n O C -> Text
toDotText bd =
  let bdGr = toGr bd
      dotG = toDotGraph bdGr
      dotCode = toDot dotG
  in DTL.toStrict $ renderDot dotCode

toGr :: NonLocal n => CH.Graph n O C -> Gr (Block n C C) ()
toGr bd =
  let lblToNode l = read (drop 1 $ toS $ showT l)
      blocks = postorder_dfs bd
      (nList, eList) =
        foldr
          (\blk (nList', eList') ->
             let node = lblToNode $ entryLabel blk
                 edgs = map (\l -> (node, lblToNode l, ())) (successors blk)
             in (nList' <> [(node, blk)], eList' <> edgs))
          ([], [])
          blocks
  in mkGraph nList eList

visParams ::
     (ShowText (PBlock n)) => GraphvizParams p (Block n C C) el () (Block n C C)
visParams =
  nonClusteredParams
  { fmtNode =
      \(_, nl) ->
        [textLabel (DTL.replace "\n" "\\l" $ toS $ showText $ PBlock nl), shape BoxShape]
  }

toDotGraph :: (ShowText (PBlock n)) => Gr (Block n C C) () -> DotGraph Node
toDotGraph = graphToDot visParams
