module Journal
  ( loadJournal
  , reloadJournal
  , defaultJournalPath
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.IORef (IORef, writeIORef)
import System.Environment (lookupEnv)
import System.Directory (getHomeDirectory, doesFileExist)
import System.FilePath ((</>))

import Hledger
  ( Journal
  , readJournalFile
  , definputopts
  )

-- | Get the default journal path
-- Priority: LEDGER_FILE env var > ~/.hledger.journal
defaultJournalPath :: IO FilePath
defaultJournalPath = do
  envPath <- lookupEnv "LEDGER_FILE"
  case envPath of
    Just path -> return path
    Nothing -> do
      home <- getHomeDirectory
      let defaultPath = home </> ".hledger.journal"
      exists <- doesFileExist defaultPath
      if exists
        then return defaultPath
        else return defaultPath  -- Return default even if doesn't exist

-- | Load a journal from a file path
loadJournal :: MonadIO m => FilePath -> m (Either String Journal)
loadJournal path = liftIO $ do
  result <- runExceptT $ readJournalFile definputopts path
  case result of
    Left err      -> return $ Left err
    Right journal -> return $ Right journal

-- | Reload the journal into an IORef
reloadJournal :: MonadIO m => FilePath -> IORef Journal -> m (Either String ())
reloadJournal path ref = liftIO $ do
  result <- loadJournal path
  case result of
    Left err -> return $ Left err
    Right journal -> do
      writeIORef ref journal
      return $ Right ()
