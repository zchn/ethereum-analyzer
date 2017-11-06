{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs,
  NoImplicitPrelude, Rank2Types, TypeFamilies, UndecidableInstances
  #-}

module Ethereum.Analyzer.EVM.Util
  ( disasmToDotText
  , disasmToDotText2
  ) where

import Protolude hiding (show)

import Ethereum.Analyzer.EVM.CfgAugWithTopNPass
import Ethereum.Analyzer.EVM.CfgAugmentPass
import Ethereum.Analyzer.EVM.Disasm
import Ethereum.Analyzer.EVM.IR
import Ethereum.Analyzer.Util

disasmToDotText
  :: HasEvmBytecode a
  => a -> Text
disasmToDotText a =
  let disasmd = disasm a
      result =
        unWordLabelMapM $ do
          contract <- evmOps2HplContract disasmd
          toDotText <$> (ctorOf <$> doCfgAugmentPass contract)
  in result

disasmToDotText2
  :: HasEvmBytecode a
  => a -> (Text, Text)
disasmToDotText2 a =
  let result =
        unWordLabelMapM $ do
          contract' <- doCfgAugWithTopNPass a
          return
            (toDotText (ctorOf contract'), toDotText (dispatcherOf contract'))
  in result
