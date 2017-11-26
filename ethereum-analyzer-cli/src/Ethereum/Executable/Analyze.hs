module Ethereum.Executable.Analyze
  ( analyzeMain
  ) where

import Protolude

import qualified Data.Text as T

import Data.Time.Clock
import Data.Time.Format

import Ethereum.Analyzer.Debug
import Ethereum.Analyzer.Solidity hiding (value)

import Options.Applicative
import Options.Applicative.Text

import System.Directory
import System.FilePath

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
                           , debug = debug} = do
  tmpDirname <- getTmpDirname
  let sessionDir = toS theWorkDir </> toS tmpDirname
  createDirectoryIfMissing True sessionDir
  when debug $ putText $ show flags
  content <-
    if theAstJson == "" || theAstJson == "-"
      then getContents
      else readFile $ toS theAstJson
  case decodeContracts content of
    Right contracts -> do
      savePrettyContracts contracts (toS $
                                     sessionDir </> "contracts.ir")
      when debug $ pprintContracts contracts
      putText "Findings: \n"
      putText ("\n" `T.intercalate` concatMap findingsFor contracts)
    Left err -> putText err
  return ()

getTmpDirname :: IO Text
getTmpDirname = do
  t <- getCurrentTime
  let formated = formatTime defaultTimeLocale (
        iso8601DateFormat (Just "%H:%M:%S")) t
  return $ toS formated

savePrettyContracts :: [Contract] -> Text -> IO ()
savePrettyContracts cs filepath =
  writeFile (toS filepath) (prettyContracts cs)
