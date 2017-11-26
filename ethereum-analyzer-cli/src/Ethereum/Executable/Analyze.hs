module Ethereum.Executable.Analyze
  ( analyzeMain
  ) where

import Protolude

import qualified Data.Text as T

import Ethereum.Analyzer.Debug
import Ethereum.Analyzer.Solidity hiding (value)

import Options.Applicative
import Options.Applicative.Text

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
    (long "workDir" <> value "" <> metavar "PATH" <>
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
                           , debug = debug} = do
  when debug $ putText $ show flags
  content <-
    if theAstJson == "" || theAstJson == "-"
      then getContents
      else readFile $ toS theAstJson
  case decodeContracts content of
    Right contracts -> do
      when debug $ pprintContracts contracts
      putText "Findings: \n"
      putText ("\n" `T.intercalate` concatMap findingsFor contracts)
    Left err -> putText err
  return ()
