{-# LANGUAGE OverloadedStrings, FlexibleContexts,
  FlexibleInstances, GADTs, Rank2Types, DeriveGeneric, TypeFamilies,
  UndecidableInstances #-}

module Blockchain.Analyze.Util
  ( toDotText
  ) where

import Blockchain.Analyze.IR
import Compiler.Hoopl
import Data.GraphViz

-- import Data.GraphViz.Attributes
import Data.GraphViz.Printing
import Data.Graph.Inductive.Graph as DGIG
import Data.Graph.Inductive.PatriciaTree
import Data.Text.Lazy as DTL

toDotText :: HplBody -> Text
toDotText bd =
  let bdGr = toGr bd
      dotG = toDotGraph bdGr
      dotCode = toDot dotG
  in renderDot dotCode

toGr :: HplBody -> Gr (Block HplOp C C) ()
toGr bd =
  let lblToNode l = read (Prelude.drop 1 $ show l)
      (nList, eList) =
        mapFoldWithKey
          (\lbl blk (nList', eList') ->
              let node = lblToNode lbl
                  edgs =
                    Prelude.map (\l -> (node, lblToNode l, ())) (successors blk)
              in (nList' ++ [(node, blk)], eList' ++ edgs))
          ([], [])
          bd
  in mkGraph nList eList

visParams =
  nonClusteredParams
  { fmtNode = \(_, nl) -> [toLabel $ show nl, shape BoxShape]
  }

toDotGraph :: Gr (Block HplOp C C) () -> DotGraph Node
toDotGraph gr = graphToDot visParams gr
