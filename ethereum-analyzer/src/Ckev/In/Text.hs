{-# LANGUAGE CPP, RankNTypes, GADTs, ScopedTypeVariables, FlexibleContexts #-}
module Ckev.In.Text ( ShowText(..)
                    , showT
                    , showGraphT) where

import Protolude hiding (show)

import Compiler.Hoopl
import GHC.Show (Show(..))

showT :: (Show a) => a -> Text
showT = toS . show

class ShowText a where
  showText :: a -> Text

-- Compiler.Hoopl

type ShowingT n = forall e x . n e x -> Text

showGraphT :: forall n e x . ShowingT n -> Graph n e x -> Text
showGraphT n = toS . (showGraph $ toS . n)
