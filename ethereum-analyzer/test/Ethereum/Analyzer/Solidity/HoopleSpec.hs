module Ethereum.Analyzer.Solidity.HoopleSpec
  ( spec
  ) where

import Protolude hiding (show)

import Compiler.Hoopl

import Ethereum.Analyzer.Solidity
import Ethereum.Analyzer.TestData.Asts
import GHC.Show (Show(..))

import Test.Hspec


spec :: Spec
spec = do
  describe "hoopleOf" $ mapM_ hoopleOfWorks testFilepaths


hoopleOfWorks :: Text -> SpecWith ()
hoopleOfWorks filepath =
  context (toS filepath) $
  it "converts Contract to HContract" $ do
    cfgs <- do -- IO
      solNodes <- solNodesOf filepath
      return $ runSimpleUniqueMonad $ do -- UniqueMonad
        contracts <- concat <$> mapM s2sContracts solNodes
        hContracts <- mapM hoopleOf contracts
        return $ map hfCFG $ concatMap hcFunctions hContracts
    sum (map (\g -> foldGraphNodes (const (1 +)) g 0) cfgs)
      `shouldSatisfy` (> 0)
