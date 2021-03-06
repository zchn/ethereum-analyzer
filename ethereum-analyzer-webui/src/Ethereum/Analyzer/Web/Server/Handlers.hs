{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- | Implementation of the ethereum-analyzer API.
module Ethereum.Analyzer.Web.Server.Handlers
  ( server
  ) where

import Protolude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Log (Severity, logInfo)
import Ethereum.Analyzer.EVM
import Ethereum.Analyzer.Web.API
       (API, DotCfgResp(..), RootPage(..), User(..), Users(..))
import qualified Ethereum.Analyzer.Web.Server.Logging as Log
import Servant
       ((:<|>)(..), (:>), (:~>)(..), Raw, ServantErr, Server, enter)
import Servant.Utils.StaticFiles (serveDirectory)
import Text.PrettyPrint.Leijen.Text (Doc, Pretty, text)

-- | ethereum-analyzer API implementation.
server :: Severity -> Server (API :<|> "web" :> Raw)
server logLevel = enter (toHandler logLevel) handlers :<|> serveDirectory "web"
  where
    handlers = pure RootPage :<|> users :<|> dotcfg :<|> dotcfg2

-- | Our custom handler type.
type Handler msg = ExceptT ServantErr (Log.LogM msg IO)

-- | Translate our custom monad into a Servant handler.
--
-- See http://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html#using-another-monad-for-your-handlers
-- for the details.
toHandler
  :: Pretty msg
  => Severity -> (Handler msg :~> ExceptT ServantErr IO)
toHandler logLevel = Nat toHandler'
  where
    toHandler'
      :: Pretty msg
      => Handler msg a -> ExceptT ServantErr IO a
    toHandler' = ExceptT . Log.withLogging logLevel . runExceptT

-- | Example endpoint.
users :: Handler Doc Users
users = do
  logInfo (text "Example of logging")
  pure (Users [User 1 "Isaac" "Newton", User 2 "Albert" "Einstein"])

dotcfg :: Maybe Text -> Handler Doc DotCfgResp
dotcfg (Just t) = pure (DotCfgResp (disasmToDotText $ EvmHexString t) "")
dotcfg _ = pure (DotCfgResp "" "")

dotcfg2 :: Maybe Text -> Handler Doc DotCfgResp
dotcfg2 (Just t) = pure (uncurry DotCfgResp $ disasmToDotText2 $ EvmHexString t)
dotcfg2 _ = pure (DotCfgResp "" "")
