module Main
  ( main
  ) where

import Protolude

import Ethereum.Analyzer.Debug

import Options.Applicative
import Options.Applicative.Text

data AnalyzeFlags = AnalyzeFlags
                    { astJson :: Text
                    , debug   :: Bool }
                    deriving (Eq, Show)

analyzeFlags :: Parser AnalyzeFlags
analyzeFlags = AnalyzeFlags
  <$> textOption
  ( long "astJson"
    <> metavar "PATH"
    <> help "Path to the ast-json file." )
  <*> switch
  ( long "debug"
    <> help "Whether to print debug info" )

main :: IO ()
main = analyze =<< execParser opts
  where
    opts = info (analyzeFlags <**> helper)
           ( fullDesc
             <> progDesc (toS ("Analyze the contract specified at PATH" :: Text))
             <> header (toS ("ea-analyze - CLI interface for ethereum-analyzer" :: Text)))

analyze :: AnalyzeFlags -> IO ()
analyze flags@AnalyzeFlags { astJson = theAstJson } = do
  putText $ show flags
  content <- readFile $ toS theAstJson
  -- putText content
  pprintSimpleSol content
  return ()
