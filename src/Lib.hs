module Lib
  ( -- * Server
    runServer
  , mkApp
    -- * Re-exports
  , module App
  , module Api
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.IORef (newIORef)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Server.Generic (AsServerT)

import qualified Hledger as H

import Api
import Api.Accounts (accountsHandlers)
import Api.Reports (reportsHandlers, metaHandlers)
import Api.Swagger (apiSwagger)
import Api.Transactions (transactionsHandlers)
import App

-- | Run the server with the given config
runServer :: AppConfig -> IO ()
runServer config = do
  putStrLn $ "Loading journal from: " ++ configJournalPath config
  journalResult <- runExceptT $ H.readJournalFile H.definputopts (configJournalPath config)
  case journalResult of
    Left err -> do
      putStrLn $ "Error loading journal: " ++ err
      error "Failed to load journal"
    Right journal -> do
      journalRef <- newIORef journal
      let env = AppEnv config journalRef
      putStrLn $ "Starting server on " ++ configHost config ++ ":" ++ show (configPort config)
      run (configPort config) (mkApp env)

-- | Create the WAI application
mkApp :: AppEnv -> Application
mkApp env = serve (Proxy :: Proxy API) (hoistServer (Proxy :: Proxy API) (nt env) server)
  where
    nt :: AppEnv -> AppM a -> Handler a
    nt e action = liftIO $ runAppM e action

-- | Full server implementation
server :: ServerT API AppM
server = rootServer :<|> return apiSwagger

-- | Root API server with named routes
rootServer :: RootAPI (AsServerT AppM)
rootServer = RootAPI
  { accounts     = accountsHandlers
  , transactions = transactionsHandlers
  , reports      = reportsHandlers
  , meta         = metaHandlers
  }
