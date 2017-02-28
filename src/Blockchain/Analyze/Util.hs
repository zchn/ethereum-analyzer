{-# LANGUAGE OverloadedStrings, FlexibleContexts,
  OverloadedStrings, FlexibleInstances, GADTs, Rank2Types,
  DeriveGeneric, TypeFamilies, UndecidableInstances #-}

module Blockchain.Analyze.Util
  ( toDotText
  , decompileToDotText
  ) where

import Blockchain.Analyze.Decompile
import Blockchain.Analyze.IR
import Blockchain.Analyze.CfgAugmentPass
import Compiler.Hoopl
import Data.ByteString.Char8 as DBC
import Data.GraphViz
import Data.GraphViz.Printing
import Data.Graph.Inductive.Graph as DGIG
import Data.Graph.Inductive.PatriciaTree
import Data.Text as DT
import qualified Data.Text.Lazy as DTL

decompileToDotText :: Text -> Text
decompileToDotText hexcode =
  let decompiled@((loc, _):_) = decompileHexString $ DBC.pack $ DT.unpack hexcode
      result =
        unWordLabelMapM $
        do entry <- labelFor loc
           body <- evmOps2HplBody decompiled
           toDotText <$> doCfgAugmentPass entry body
  in result

toDotText :: HplBody -> Text
toDotText bd =
  let bdGr = toGr bd
      dotG = toDotGraph bdGr
      dotCode = toDot dotG
  in DTL.toStrict $ renderDot dotCode

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
