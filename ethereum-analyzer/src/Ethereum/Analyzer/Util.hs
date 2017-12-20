{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs,
  NoImplicitPrelude, Rank2Types, TypeFamilies, UndecidableInstances
  #-}

module Ethereum.Analyzer.Util
  ( toDotText
  ) where

import Protolude hiding (show)

import Compiler.Hoopl as CH
import Data.Graph.Inductive.Graph as DGIG
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz
import Data.GraphViz.Printing hiding ((<>))
import qualified Data.Text.Lazy as DTL
import qualified Data.List as DL
import GHC.Show
import Text.Read (read)

instance ( Show (n C O)
         , Show (n O O)
         , Show (n O C)) => Show (Block n C C) where
  show a =
    let (h, m, t) = blockSplit a
    in DL.unlines $ [show h] <> map show (blockToList m) <> [show t]

toDotText
  :: (NonLocal n, Show (Block n C C))
  => CH.Graph n O C -> Text
toDotText bd =
  let bdGr = toGr bd
      dotG = toDotGraph bdGr
      dotCode = toDot dotG
  in DTL.toStrict $ renderDot dotCode

toGr
  :: NonLocal n
  => CH.Graph n O C -> Gr (Block n C C) ()
toGr bd =
  let lblToNode l = read (drop 1 $ toS $ show l)
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

visParams
  :: (Show (Block n C C))
  => GraphvizParams p (Block n C C) el () (Block n C C)
visParams =
  nonClusteredParams
  {fmtNode = \(_, nl) -> [textLabel (DTL.replace "\n" "\\l" $ toS $ show nl), shape BoxShape]}

toDotGraph
  :: (Show (Block n C C))
  => Gr (Block n C C) () -> DotGraph Node
toDotGraph = graphToDot visParams
