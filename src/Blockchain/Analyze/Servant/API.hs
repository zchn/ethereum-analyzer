{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

-- | API definition for ethereum-analyzer.
module Blockchain.Analyze.Servant.API
  ( API
  , apiraw
  , RootPage(..)
  , User(User)
  , Users(..)
  , DotCfgResp(..)
  ) where

import Protolude

import Data.Aeson
       (FromJSON(..), ToJSON(..), Value(..), object, (.=), (.:))
import Data.Aeson.Types (typeMismatch)
import qualified NeatInterpolation as NI
import Servant.API
       ((:>), (:<|>)(..), Get, JSON, MimeRender(..), QueryParam, Raw)

import Blockchain.Analyze.Servant.API.Internal (HTML)

-- | ethereum-analyzer API definition.
type API = Get '[HTML] RootPage :<|> "users" :> Get '[JSON] Users
  :<|> "ea" :> "dotcfg" :> QueryParam "code" Text :> Get '[JSON] DotCfgResp
  :<|> "ea" :> "dotcfg2" :> QueryParam "code" Text :> Get '[JSON] DotCfgResp

-- | Value-level representation of API.
apiraw :: Proxy (API :<|> "web" :> Raw)
apiraw = Proxy

-- | Example object. Replace this with something relevant to your app.
data User = User
  { _userId :: Int
  , _userFirstName :: Text
  , _userLastName :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON User

instance ToJSON User

data DotCfgResp = DotCfgResp
  { _dotcfg :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON DotCfgResp

instance ToJSON DotCfgResp

-- | Represents a list of users.
--
-- We have a newtype so we can be sure to return a JSON object.
newtype Users =
  Users [User]
  deriving (Eq, Show, Generic)

instance FromJSON Users where
  parseJSON (Object v) = Users <$> v .: "users"
  parseJSON x = typeMismatch "Users" x

instance ToJSON Users where
  toJSON (Users users) = object ["users" .= toJSON users]

-- | Represents the root page of the service.
data RootPage =
  RootPage

-- | Very simple root HTML page.
instance MimeRender HTML RootPage where
  mimeRender _ _ =
    toS
      [NI.text|
         <!doctype html>
         <html>
         <head><title>ethereum-analyzer</title></head>
         <body>
         <h1>ethereum-analyzer</h1>
         <ul>
         <li><a href="/users">users</a></li>
         <li><a href="/ea/dotcfg">/ea/dotcfg</a></li>
         <li><a href="/metrics"><code>/metrics</code></a></li>
         </ul>
         <p>
         Source code at <a href="https://github.com/ethereumK/ethereum-analyzer">https://github.com/ethereumK/ethereum-analyzer/</a>
         </p>
         </body>
         <html>
         |]
