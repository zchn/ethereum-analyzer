module Ethereum.Executable.Analyze
  ( analyzeMain
  ) where

import Protolude hiding ((<.>))

import Compiler.Hoopl (runSimpleUniqueMonad)

import Data.Time.Clock
import Data.Time.Format

import Ethereum.Analyzer.Debug
import Ethereum.Analyzer.Solidity hiding (value)
import Ethereum.Analyzer.Util

import Options.Applicative
import Options.Applicative.Text

import System.Directory
import System.FilePath

import qualified Data.Text as T

data AnalyzeFlags = AnalyzeFlags
  { astJson :: Text
  , workDir :: Text
  , debug :: Bool
  } deriving (Eq, Show)

analyzeFlags :: Parser AnalyzeFlags
analyzeFlags =
  AnalyzeFlags <$>
  textOption
    (long "astJson" <> value "" <> metavar "PATH" <>
     help "Path to the ast-json file.") <*>
  textOption
    (long "workDir" <> value "work" <> metavar "PATH" <>
     help "Path to the work directory (for outputs and intermediate files).") <*>
  switch (long "debug" <> help "Whether to print debug info")

analyzeMain :: IO ()
analyzeMain = analyze =<< execParser opts
  where
    opts =
      info
        (analyzeFlags <**> helper)
        (fullDesc <>
         progDesc (toS ("Analyze the contract specified at PATH" :: Text)) <>
         header
           (toS ("ea-analyze - CLI interface for ethereum-analyzer" :: Text)))

analyze :: AnalyzeFlags -> IO ()
analyze flags@AnalyzeFlags { astJson = theAstJson
                           , workDir = theWorkDir
                           , debug = _debug
                           } = do
  tmpDirname <- getTmpDirname
  let sessionDir = toS theWorkDir </> toS tmpDirname
  createDirectoryIfMissing True sessionDir
  when _debug $ putText $ show flags
  content <-
    if theAstJson == "" || theAstJson == "-"
      then getContents
      else readFile $ toS theAstJson
  case decodeContracts content of
    Right contracts -> do
      savePrettyContracts contracts (toS $ sessionDir </> "contracts.ir")
      when _debug $ pprintContracts contracts
      saveCfgs contracts (toS $ sessionDir </> "cfgs")
      putText "Findings: \n"
      putText ("\n" `T.intercalate` concatMap findingsFor contracts)
    Left err -> putText err
  return ()

getTmpDirname :: IO Text
getTmpDirname = do
  t <- getCurrentTime
  let formated =
        formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) t
  return $ toS formated

savePrettyContracts :: [Contract] -> Text -> IO ()
savePrettyContracts cs filepath = writeFile (toS filepath) (prettyContracts cs)

saveCfgs :: [Contract] -> Text -> IO ()
saveCfgs cs dirpath = do
  createDirectoryIfMissing True (toS dirpath)
  let hcs = runSimpleUniqueMonad $ mapM hoopleOf cs
  _ <- mapM writeContractCfgs hcs
  return ()
  where
    writeContractCfgs hc = do
      _ <-
        mapM (writeFunCfgs (toS dirpath </> toS (hcName hc))) (hcFunctions hc)
      return ()
    writeFunCfgs contractPrefix hf = do
      let dot = toDotText (hfCFG hf)
      writeFile
        (contractPrefix <.> (toS $ unIdfr $ hfName hf) <.> "CFG" <.> ".dot")
        dot
