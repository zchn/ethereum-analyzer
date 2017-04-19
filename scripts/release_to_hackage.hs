#!/usr/bin/env stack
{- stack --install-ghc
    runghc
    --package Command
    --package text
    --package hflags
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Text
-- import Data.Version
import HFlags
import System.Command

defineFlag "version" ("0.0.1" :: Text) "version."
defineFlag "name" ("Indiana Jones" :: Text) "Who to greet."

main :: IO ()
main = do _ <- $initHFlags "release_to_hackage.hs"
          putStrLn $ "Updating to version  " ++ show flags_version
          _ <- system "ls"
          return ()
