#!/usr/bin/env stack
{- stack --install-ghc
    runghc
    --package Command
    --package text
    --package hflags
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.Version
import HFlags
import System.Command
import Text.ParserCombinators.ReadP

defineFlag "version" ("0.0.1" :: String) "version."
defineFlag "name" ("Indiana Jones" :: String) "Who to greet."

systemOrDie :: String -> IO ()
systemOrDie cmd = do
  result <- system cmd
  when (isFailure result) $ error $ "Failed: " ++ cmd
  return ()

packages :: [String]
packages = ["ethereum-analyzer",
            "ethereum-analyzer-webui",
            "ethereum-analyzer-cli"]

main :: IO ()
main = do _ <- $initHFlags "release_to_hackage.hs"
          case reverse $ readP_to_S parseVersion flags_version of
            (Version v@[_,_,_] [], "") : _ -> do
              putStrLn $ "Updating to version  " ++ show v
              systemOrDie "git checkout master"
              systemOrDie "git stash"
              _ <- mapM (
                \package ->
                  systemOrDie $
                    "sed -i'' " ++
                    "'s/^version:\\([[:blank:]]*\\)[[:digit:]]\\+.[[:digit:]]\\+.[[:digit:]]\\+" ++
                    "/version:\\1" ++ flags_version ++ "/' " ++ package ++ "/" ++
                    package ++ ".cabal") packages
              systemOrDie "git diff"
              -- systemOrDie $ "git tag v" + flags_version
            other -> error $ "malformed version: " ++ flags_version ++ " " ++ show other
          return ()
