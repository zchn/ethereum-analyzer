{-# LANGUAGE OverloadedStrings, FlexibleContexts,
  OverloadedStrings, FlexibleInstances, GADTs, Rank2Types,
  DeriveGeneric, TypeFamilies, UndecidableInstances #-}

module Ethereum.Analyzer.Util
  ( toDotText
  , decompileToDotText
  , decompileToDotText2
  ) where

import Ethereum.Analyzer.Decompile
import Ethereum.Analyzer.IR
import Ethereum.Analyzer.CfgAugWithTopNPass
import Ethereum.Analyzer.CfgAugmentPass
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
  let decompiled = decompileHexString $ DBC.pack $ DT.unpack hexcode
      result =
        unWordLabelMapM $
        do contract <- evmOps2HplContract decompiled
           toDotText <$> (bodyOf . ctorOf <$> doCfgAugmentPass contract)
  in result

decompileToDotText2 :: Text -> (Text, Text)
decompileToDotText2 hexcode =
  let hexstring = DBC.pack $ DT.unpack hexcode
      result =
        unWordLabelMapM $
        do contract' <- doCfgAugWithTopNPass hexstring
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
