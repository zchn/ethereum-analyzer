{-# LANGUAGE FlexibleContexts, OverloadedStrings, TemplateHaskell #-}

module Blockchain.Jsonrpc.Client
  (jrpcVersion) where

import Conduit
import Control.Monad.Logger
import Data.Aeson
import Data.Aeson.Types hiding (Error)
import Data.ByteString.Char8 as DBC
import Data.Conduit.Network
import Data.Text as T
import Network.HTTP.Simple as NHS
import Network.JsonRpc as NJ

data Req = Web3_clientVersionReq deriving (Show, Eq)

instance FromRequest Req where
  parseParams "web3_clientVersion" = Just $ const $ return Web3_clientVersionReq
  parseParams _ = Nothing

instance ToRequest Req where
  requestMethod Web3_clientVersionReq = "web3_clientVersion"
  requestIsNotif        = const False

instance ToJSON Req where
  toJSON = const emptyArray

data Res = Web3_clientVersionRes Text deriving (Show, Eq)

instance FromResponse Res where
  parseResult "web3_clientVersion" = Just $ withText "web3_clientVersion" (
    return . Web3_clientVersionRes)
  parseResult _ = Nothing

instance ToJSON Res where
  toJSON (Web3_clientVersionRes result) = toJSON result

jrpcVersion :: (MonadIO m) => String -> Int -> m Text
jrpcVersion server port =
  initReq <- NHS.parseRequest ("POST http://" ++ server ++ ":" ++ (show port))
  let req = NHS.setRequestBodyJSON
      (NJ.buildRequest V2 Web3_clientVersionReq 1) initReq
  resp <- NHS.httpJSON req
  case getResponseStatus resp of
    NHS.ok200 ->
      case fromResponse "web3_clientVersion" (getResponseBody resp) of
          Just $ Web3_clientVersionRes ver -> return ver
          Nothing -> error $ "couldn't get version from response: " ++ (show resp)
    other -> error $ "non-OK status when getting version: " ++ (show other)

-- handleResponse :: Maybe (Either ErrorObj Res) -> Res
-- handleResponse t =
--   case t of
--     Nothing -> error "could not receive or parse response"
--     Just (Left e) -> error $ fromError e
--     Just (Right r) -> r

-- jrpcVersion_ :: MonadLoggerIO m => JsonRpcT m Res
-- jrpcVersion_ = do
--   $(logDebug) "debugging response parsing"
--   $(logDebug) $ T.pack $ "toJSON: " ++ (show $ toJSON $ Web3_clientVersionRes "versssss")
--   $(logDebug) $ T.pack $ "parsing: " ++ (show ((fromResponse "web3_clientVersion" $ Response V2 (toJSON $ Web3_clientVersionRes "versssss") (IdInt 1)) :: Maybe Res))
--   -- $(logDebug) $ T.pack $ "fromResponse toJSON: " ++ (toJSON $ Web3_clientVersionRes "versssss")
--   $(logDebug) "sending web3_clientVersion request"
--   tEM <- sendRequest Web3_clientVersionReq
--   return $ handleResponse tEM


-- jrpcVersion :: (MonadLoggerIO m, MonadBaseControl IO m) => String -> Int -> m Text
-- jrpcVersion server port = do
--   $logInfo $ T.pack $ "Conntectin to " ++ server ++ ":" ++ show port
--   Web3_clientVersionRes ver <- jsonRpcTcpClient V2 True (
--     clientSettings port $ DBC.pack server) jrpcVersion_
--   return ver
