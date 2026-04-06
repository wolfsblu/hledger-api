module Api.Import
  ( handleImportTransactions
  ) where

import Control.Exception (bracket, catch, try, SomeException)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Except (runExceptT)
import Data.List (partition, sort)
import Data.Text (Text)
import Data.Time (Day)
import Servant
import Servant.Multipart
import System.Directory (doesFileExist, getTemporaryDirectory, listDirectory, removeFile)
import System.FilePath ((</>), takeBaseName, takeExtension)
import System.IO (hClose, openTempFile)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Hledger as H

import Api.Convert (toTransactionJSON)
import Api.Types
import App (AppM, AppConfig(..), AppEnv(..), getJournal, modifyJournal)

-- | Handle CSV import request
handleImportTransactions
  :: Text               -- ^ rules name (e.g. "bank-checking")
  -> MultipartData Mem  -- ^ multipart upload
  -> AppM ImportResponse
handleImportTransactions rulesName multipartData = do

  -- 1. Extract CSV bytes from the "file" field
  let fileList = files multipartData
  fd <- case filter (\f -> fdInputName f == "file") fileList of
    []    -> throwError err400
               { errBody = Aeson.encode $ Aeson.object
                   [ "error" Aeson..= ("Missing 'file' field in multipart upload" :: Text) ]
               , errHeaders = [("Content-Type", "application/json")]
               }
    (f:_) -> pure f
  let csvBytes = fdPayload fd

  -- 2. Resolve and validate rules file
  rulesDir <- asks (configRulesDir . envConfig)
  let rulesPath = rulesDir </> T.unpack rulesName <> ".csv.rules"
  rulesExists <- liftIO $ doesFileExist rulesPath
  unless rulesExists $ do
    available <- liftIO $ listAvailableRules rulesDir
    throwError err400
      { errBody = Aeson.encode $ Aeson.object
          [ "error"     Aeson..= ("Rules file not found: " <> rulesName :: Text)
          , "rulesDir"  Aeson..= T.pack rulesDir
          , "available" Aeson..= available
          ]
      , errHeaders = [("Content-Type", "application/json")]
      }

  -- 3. Write CSV bytes to a temp file, parse with hledger, then clean up
  let opts = H.definputopts { H.mrules_file_ = Just rulesPath }
  parseResult <- liftIO $ withTempCsvFile csvBytes $ \csvPath ->
    runExceptT $ H.readJournalFile opts csvPath

  csvJournal <- case parseResult of
    Left err -> throwError err400
      { errBody = Aeson.encode $ Aeson.object
          [ "error"   Aeson..= ("CSV parse error" :: Text)
          , "details" Aeson..= err
          ]
      , errHeaders = [("Content-Type", "application/json")]
      }
    Right j  -> pure j

  -- 4. Deduplicate: skip any transaction already in the journal
  journal <- getJournal
  let existingFps = Set.fromList $ map txnFingerprint (H.jtxns journal)
      (toImport, skipped) = partition
        (\t -> txnFingerprint t `Set.notMember` existingFps)
        (H.jtxns csvJournal)

  -- 5. Append new transactions to journal file
  journalPath <- asks (configJournalPath . envConfig)
  let txnTexts = foldMap (\t -> "\n" <> H.showTransaction t) toImport
  writeResult <- liftIO $ try @SomeException $
    appendFile journalPath (T.unpack txnTexts)
  case writeResult of
    Left ex ->
      throwError err500 { errBody = "Failed to write to journal: " <> Aeson.encode (show ex) }
    Right () -> pure ()

  -- 6. Update in-memory journal
  modifyJournal (\j -> j { H.jtxns = H.jtxns j ++ toImport })

  -- 7. Build and return response
  let startIdx = length (H.jtxns journal)
      indexed  = zipWith toTransactionJSON [startIdx..] toImport
  return ImportResponse
    { importedCount = length toImport
    , skippedCount  = length skipped
    , importedTxns  = indexed
    }

-- | List rule names (without extension) available in the given directory
listAvailableRules :: FilePath -> IO [Text]
listAvailableRules dir = do
  entries <- listDirectory dir `catch` \(_ :: SomeException) -> return []
  return $ map (T.pack . takeBaseName) $ filter (\f -> takeExtension f == ".rules") entries

-- | Write bytes to a temp file, run an action with the path, then delete the file
withTempCsvFile :: LBS.ByteString -> (FilePath -> IO a) -> IO a
withTempCsvFile bytes action = do
  tmpDir <- getTemporaryDirectory
  bracket
    (do (path, h) <- openTempFile tmpDir "hledger-import-.csv"
        LBS.hPut h bytes
        hClose h
        return path)
    (\path -> removeFile path `catch` \(_ :: SomeException) -> return ())
    action

-- | Deduplication fingerprint: date + description + sorted (account, amount text)
txnFingerprint :: H.Transaction -> (Day, Text, [(Text, Text)])
txnFingerprint t =
  ( H.tdate t
  , H.tdescription t
  , sort [(H.paccount p, T.pack $ show $ H.pamount p) | p <- H.tpostings t]
  )
