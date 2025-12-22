{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App
  ( AppConfig(..)
  , AppEnv(..)
  , AppM(..)
  , runAppM
  , getJournal
  , defaultConfig
  ) where

import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import qualified Control.Exception as Control.Exception
import Data.IORef (IORef, readIORef)
import Hledger (Journal)
import Servant.Server (ServerError)

-- | Application configuration
data AppConfig = AppConfig
  { configJournalPath :: FilePath
  , configPort        :: Int
  , configHost        :: String
  } deriving (Show, Eq)

-- | Default configuration
defaultConfig :: AppConfig
defaultConfig = AppConfig
  { configJournalPath = ""  -- Will be resolved from env/args
  , configPort        = 8080
  , configHost        = "127.0.0.1"
  }

-- | Application environment (runtime state)
data AppEnv = AppEnv
  { envConfig  :: AppConfig
  , envJournal :: IORef Journal
  }

-- | Application monad
newtype AppM a = AppM { unAppM :: ReaderT AppEnv IO a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader AppEnv
    )

-- | MonadError instance for throwing Servant errors
instance MonadError ServerError AppM where
  throwError = liftIO . Control.Exception.throwIO
  catchError action handler = AppM $ do
    env <- asks id
    liftIO $ Control.Exception.catch (runAppM env action) (\e -> runAppM env (handler e))

-- | Run an AppM action
runAppM :: AppEnv -> AppM a -> IO a
runAppM env action = runReaderT (unAppM action) env

-- | Get the current journal from the environment
getJournal :: AppM Journal
getJournal = do
  ref <- asks envJournal
  liftIO $ readIORef ref
