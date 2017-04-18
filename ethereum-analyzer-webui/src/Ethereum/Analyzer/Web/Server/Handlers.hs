{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- | Implementation of the ethereum-analyzer API.
module Ethereum.Analyzer.Web.Server.Handlers
  ( server
  ) where

-- XXX: jml doesn't like the name "Handlers" for this, and isn't sure that it
-- should be in a submodule of Project.Server. Perhaps the code in
-- Project.Server (which is command-line processing, setting up logs &
-- monitoring, starting the HTTP server) should be in a different module.
import Protolude hiding (Handler)

import Ethereum.Analyzer.Util
import Ethereum.Analyzer.Web.API
       (API, RootPage(..), User(..), Users(..), DotCfgResp(..))
import qualified Ethereum.Analyzer.Web.Server.Logging as Log
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Log (Severity, logInfo)
import qualified Data.Text.Lazy as DTL
import Servant
       (ServantErr, Server, (:<|>)(..), (:>), (:~>)(..), enter, Raw)
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
dotcfg (Just t) = pure (DotCfgResp (decompileToDotText t) "")
dotcfg _ = pure (DotCfgResp "" "")

dotcfg2 :: Maybe Text -> Handler Doc DotCfgResp
dotcfg2 (Just t) = pure (uncurry DotCfgResp $ decompileToDotText2 t)
dotcfg2 _ = pure (DotCfgResp "" "")
