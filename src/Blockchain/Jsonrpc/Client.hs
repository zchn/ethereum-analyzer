{-# LANGUAGE FlexibleContexts, OverloadedStrings, TemplateHaskell #-}

module Blockchain.Jsonrpc.Client
  (web3ClientVersion) where

import Conduit
import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.Types hiding (Error)
import Data.Text as T
import Network.HTTP.Conduit as NHC hiding (port)
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

data Res = Web3_clientVersionRes {clientVersion :: Text} deriving (Show, Eq)

instance FromResponse Res where
  parseResult "web3_clientVersion" = Just $ withText "web3_clientVersion" (
    return . Web3_clientVersionRes)
  parseResult _ = Nothing

instance ToJSON Res where
  toJSON (Web3_clientVersionRes result) = toJSON result

callJsonRpc :: (MonadIO m, MonadCatch m) => String -> Int -> Req -> m Res
callJsonRpc server port req = do
  initReq <- NHC.parseUrl ("http://" ++ server ++ ":" ++ (show port))
  let requ = initReq {
                    NHC.method = "POST",
                    NHC.requestHeaders = ("Content-Type", "application/json") : NHC.requestHeaders initReq,
                    NHC.requestBody = RequestBodyLBS $ encode $ toJSON (NJ.buildRequest V2 req (IdInt 1))
                    }
  manager <- liftIO $ newManager tlsManagerSettings
  resp <- NHC.httpLbs requ manager
  case decode $ responseBody resp of
    Just body -> case fromResponse (requestMethod req) body of
      Just res -> return res
      Nothing -> error $ "couldn't parse json-rpc response: " ++ (show resp)
    Nothing -> error $ "couldn't parse json: " ++ (show resp)

web3ClientVersion :: (MonadIO m, MonadCatch m) => String -> Int -> m Text
web3ClientVersion server port = fmap clientVersion $ callJsonRpc server port Web3_clientVersionReq
