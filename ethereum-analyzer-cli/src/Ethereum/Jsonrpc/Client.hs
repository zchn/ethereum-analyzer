{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Ethereum.Jsonrpc.Client
  ( web3ClientVersion
  , ethBlockNumber
  , ethGetTransactionsByBlockNumber
  , ethGetContractAddrByTxHash
  , ethGetCode
  , getCode
  ) where

import Blockchain.Data.Code as BDC
import Conduit
import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.Types hiding (Error)
import qualified Data.ByteString.Char8 as DBC
import Data.Foldable as DF
import Data.HashMap.Strict as DHS
import Data.HexString
import Data.Text as T
import qualified Data.Vector as V
import Network.HTTP.Conduit as NHC hiding (port)
import Network.JsonRpc as NJ

data Req
  = Web3_clientVersionReq
  | Eth_blockNumberReq
    -- blockNumber, returnFullTransation
  | Eth_getBlockByNumberReq Text
                            Bool
    -- txHash
  | Eth_getTransactionReceiptReq Text
    -- codeAddres codeBlockNum
  | Eth_getCodeReq Text
                   Text
  deriving (Show, Eq)

parseJSONElemAtIndex
  :: FromJSON a
  => Int -> V.Vector Value -> Parser a
parseJSONElemAtIndex idx ary = parseJSON (V.unsafeIndex ary idx)

instance FromRequest Req where
  parseParams "web3_clientVersion" = Just $ const $ return Web3_clientVersionReq
  parseParams "eth_blockNumber" = Just $ const $ return Eth_blockNumberReq
  parseParams "eth_getBlockByNumber" =
    Just $
    withArray "(blockNumber, returnFullTransation)" $ \ab ->
      let n = V.length ab
      in if n == 2
           then do
             bn <- parseJSONElemAtIndex 0 ab
             full <- parseJSONElemAtIndex 1 ab
             return $ Eth_getBlockByNumberReq bn full
           else fail $
                "cannot unpack array of length " ++
                show n ++ " into a Eth_getBlockByNumberReq"
  parseParams "eth_getTransactionReceipt" =
    Just $
    withArray "(txHash)" $ \ab ->
      let n = V.length ab
      in if n == 1
           then do
             txhash <- parseJSONElemAtIndex 0 ab
             return $ Eth_getTransactionReceiptReq txhash
           else fail $
                "cannot unpack array of length " ++
                show n ++ " into a Eth_getTransactionReceiptReq"
  parseParams "eth_getCode" =
    Just $
    withArray "(address, blockNum)" $ \ab ->
      let n = V.length ab
      in if n == 2
           then do
             addr <- parseJSONElemAtIndex 0 ab
             blk <- parseJSONElemAtIndex 1 ab
             return $ Eth_getCodeReq addr blk
           else fail $
                "cannot unpack array of length " ++
                show n ++ " into a Eth_getCodeReq"
  parseParams _ = Nothing

instance ToRequest Req where
  requestMethod Web3_clientVersionReq = "web3_clientVersion"
  requestMethod Eth_blockNumberReq = "eth_blockNumber"
  requestMethod (Eth_getBlockByNumberReq _ _) = "eth_getBlockByNumber"
  requestMethod (Eth_getTransactionReceiptReq _) = "eth_getTransactionReceipt"
  requestMethod (Eth_getCodeReq _ _) = "eth_getCode"
  requestIsNotif = const False

instance ToJSON Req where
  toJSON Web3_clientVersionReq = emptyArray
  toJSON Eth_blockNumberReq = emptyArray
  toJSON (Eth_getBlockByNumberReq blk full) = toJSON (blk, full)
  toJSON (Eth_getTransactionReceiptReq txhash) = toJSON [txhash]
  toJSON (Eth_getCodeReq addr blk) = toJSON (addr, blk)

data Res
  = Web3_clientVersionRes { clientVersion :: Text}
  | Eth_blockNumberRes { blockNumber :: Text}
  | Eth_getBlockByNumberRes { blockInfo :: Object}
  | Eth_getTransactionReceiptRes { txReceipt :: Object}
  | Eth_getCodeRes { code :: Text}
  deriving (Show, Eq)

instance FromResponse Res where
  parseResult "web3_clientVersion" =
    Just $ withText "clientVersion" (return . Web3_clientVersionRes)
  parseResult "eth_blockNumber" =
    Just $ withText "blockNumber" (return . Eth_blockNumberRes)
  parseResult "eth_getBlockByNumber" =
    Just $ withObject "result" (return . Eth_getBlockByNumberRes)
  parseResult "eth_getTransactionReceipt" =
    Just $ withObject "result" (return . Eth_getTransactionReceiptRes)
  parseResult "eth_getCode" = Just $ withText "code" (return . Eth_getCodeRes)
  parseResult _ = Nothing

instance ToJSON Res where
  toJSON (Web3_clientVersionRes result) = toJSON result
  toJSON (Eth_blockNumberRes result) = toJSON result
  toJSON (Eth_getBlockByNumberRes result) = toJSON result
  toJSON (Eth_getTransactionReceiptRes result) = toJSON result
  toJSON (Eth_getCodeRes codeRes) = toJSON codeRes

callJsonRpc
  :: (MonadIO m, MonadCatch m)
  => String -> Int -> Req -> m Res
callJsonRpc server port req = do
  initReq <- NHC.parseUrl ("http://" ++ server ++ ":" ++ (show port))
  let requ =
        initReq
        { NHC.method = "POST"
        , NHC.requestHeaders =
            ("Content-Type", "application/json") : NHC.requestHeaders initReq
        , NHC.requestBody =
            RequestBodyLBS $ encode $ toJSON (NJ.buildRequest V2 req (IdInt 1))
        }
  manager <- liftIO $ newManager tlsManagerSettings
  resp <- NHC.httpLbs requ manager
  case decode $ responseBody resp of
    Just body ->
      case fromResponse (requestMethod req) body of
        Just res -> return res
        Nothing -> error $ "couldn't parse json-rpc response: " ++ (show resp)
    Nothing -> error $ "couldn't parse json: " ++ (show resp)

web3ClientVersion
  :: (MonadIO m, MonadCatch m)
  => String -> Int -> m Text
web3ClientVersion server port =
  clientVersion <$> callJsonRpc server port Web3_clientVersionReq

ethBlockNumber
  :: (MonadIO m, MonadCatch m)
  => String -> Int -> m Text
ethBlockNumber server port =
  blockNumber <$> callJsonRpc server port Eth_blockNumberReq

ethGetTransactionsByBlockNumber
  :: (MonadIO m, MonadCatch m)
  => String -> Int -> Text -> m [Text]
ethGetTransactionsByBlockNumber server port blk =
  (Prelude.map $ \(String s) -> s) <$> (\(Array a) -> DF.toList $ a) <$>
  (lookupDefault (Array $ V.singleton (String "error")) "transactions") <$>
  blockInfo <$>
  callJsonRpc server port (Eth_getBlockByNumberReq blk False)

ethGetContractAddrByTxHash
  :: (MonadIO m, MonadCatch m)
  => String -> Int -> Text -> m (Maybe Text)
ethGetContractAddrByTxHash server port txhash =
  (\ares ->
     case ares of
       (String a) ->
         if toLower a == "null"
           then Nothing
           else Just a
       Null -> Nothing
       other -> error $ show other) <$>
  (lookupDefault (String "error") "contractAddress") <$>
  txReceipt <$>
  callJsonRpc server port (Eth_getTransactionReceiptReq txhash)

ethGetCode
  :: (MonadIO m, MonadCatch m)
  => String -> Int -> Text -> m Text
ethGetCode server port address =
  fmap code $ callJsonRpc server port (Eth_getCodeReq address "latest")

getCode
  :: (MonadIO m, MonadCatch m)
  => String -> Int -> Text -> m Code
getCode server port address = do
  textCode <- ethGetCode server port address
  return $
    BDC.Code $ toBytes (hexString (DBC.pack $ T.unpack $ T.drop 3 textCode))
