# CSV Import Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add `POST /api/v1/transactions/import/{rules}` endpoint that accepts a multipart CSV upload, parses it with a server-side rules file, deduplicates against the existing journal, and returns imported transactions.

**Architecture:** Six sequential tasks: add the dependency, add the response type, extend config + CLI, extend the API route, implement the handler, wire it up. Each task builds on the previous. No task touches more than two files.

**Tech Stack:** Haskell/Stack LTS 24.35, Servant 0.20, servant-multipart, hledger-lib 1.43

---

### Task 1: Add servant-multipart dependency

**Files:**
- Modify: `package.yaml`
- Modify: `stack.yaml` (only if servant-multipart is not in lts-24.35)

**Step 1: Add to package.yaml**

In the `dependencies:` list (after `servant-server >= 0.20`), add:

```yaml
- servant-multipart >= 0.12
```

**Step 2: Attempt build to check if it resolves**

```bash
stack build --fast 2>&1 | head -30
```

Expected: either builds cleanly, or prints "not found in snapshot" for servant-multipart.

**Step 3: If not in snapshot, add to stack.yaml extra-deps**

Only do this if step 2 failed. Look up the latest servant-multipart version on Hackage (0.12.x) and add:

```yaml
extra-deps:
  - servant-multipart-0.12.1
```

Then re-run `stack build --fast`.

Expected: build succeeds.

**Step 4: Commit**

```bash
git add package.yaml stack.yaml
git commit -m "feat: add servant-multipart dependency"
```

---

### Task 2: Add ImportResponse type

**Files:**
- Modify: `src/Api/Types/Transaction.hs`

**Step 1: Add ImportResponse to the export list**

In `src/Api/Types/Transaction.hs`, add `ImportResponse(..)` to the module exports:

```haskell
module Api.Types.Transaction
  ( TransactionJSON(..)
  , PostingJSON(..)
  , TransactionDetail
  , CreatePostingRequest(..)
  , CreateTransactionRequest(..)
  , ImportResponse(..)        -- add this line
  ) where
```

**Step 2: Add the ImportResponse type and instances at the end of the file**

Append after the `CreateTransactionRequest` instance block:

```haskell
-- | Response body for the CSV import endpoint
data ImportResponse = ImportResponse
  { importedCount :: Int
  , skippedCount  :: Int
  , importedTxns  :: [TransactionJSON]
  } deriving (Show, Eq, Generic)

instance ToJSON ImportResponse where
  toJSON r = object
    [ "imported"     .= importedCount r
    , "skipped"      .= skippedCount r
    , "transactions" .= importedTxns r
    ]

instance ToSchema ImportResponse where
  declareNamedSchema _ = do
    txnRef <- declareSchemaRef (Proxy :: Proxy TransactionJSON)
    return $ NamedSchema (Just "ImportResponse") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~ IOHM.fromList
          [ ("imported",     Inline $ mempty & type_ ?~ OpenApiInteger)
          , ("skipped",      Inline $ mempty & type_ ?~ OpenApiInteger)
          , ("transactions", Inline $ mempty & type_ ?~ OpenApiArray
                                             & items ?~ OpenApiItemsObject txnRef)
          ]
      & required .~ ["imported", "skipped", "transactions"]
```

**Step 3: Build to confirm it compiles**

```bash
stack build --fast 2>&1
```

Expected: no errors.

**Step 4: Commit**

```bash
git add src/Api/Types/Transaction.hs
git commit -m "feat: add ImportResponse type"
```

---

### Task 3: Add configRulesDir to AppConfig and wire through CLI

**Files:**
- Modify: `src/App.hs`
- Modify: `app/Main.hs`

**Step 1: Add field to AppConfig in src/App.hs**

Change:

```haskell
data AppConfig = AppConfig
  { configJournalPath :: FilePath
  , configPort        :: Int
  , configHost        :: String
  } deriving (Show, Eq)
```

To:

```haskell
data AppConfig = AppConfig
  { configJournalPath :: FilePath
  , configRulesDir    :: FilePath
  , configPort        :: Int
  , configHost        :: String
  } deriving (Show, Eq)
```

**Step 2: Update defaultConfig in src/App.hs**

Change:

```haskell
defaultConfig :: AppConfig
defaultConfig = AppConfig
  { configJournalPath = ""
  , configPort        = 8080
  , configHost        = "127.0.0.1"
  }
```

To:

```haskell
defaultConfig :: AppConfig
defaultConfig = AppConfig
  { configJournalPath = ""
  , configRulesDir    = ""
  , configPort        = 8080
  , configHost        = "127.0.0.1"
  }
```

**Step 3: Add CLI option and wiring in app/Main.hs**

Add `optRulesDir :: FilePath` to the `Options` type:

```haskell
data Options = Options
  { optJournal  :: Maybe FilePath
  , optRulesDir :: FilePath
  , optPort     :: Int
  , optHost     :: String
  }
```

Add the parser field in `optionsParser` (after the journal option):

```haskell
  <*> strOption
      ( long "rules-dir"
     <> short 'r'
     <> metavar "DIR"
     <> value "rules"
     <> showDefault
     <> help "Directory containing .csv.rules files (default: rules/)"
      )
```

Update the config construction in `main`:

```haskell
  let config = AppConfig
        { configJournalPath = journalPath
        , configRulesDir    = optRulesDir opts
        , configPort        = optPort opts
        , configHost        = optHost opts
        }
```

**Step 4: Build to confirm it compiles**

```bash
stack build --fast 2>&1
```

Expected: no errors.

**Step 5: Commit**

```bash
git add src/App.hs app/Main.hs
git commit -m "feat: add configRulesDir to AppConfig and --rules-dir CLI option"
```

---

### Task 4: Add importTransactions route to TransactionsAPI and export toTransactionJSON

**Files:**
- Modify: `src/Api.hs`
- Modify: `src/Api/Transactions.hs`

**Step 1: Add MultipartForm import to src/Api.hs**

Add to the import block:

```haskell
import Servant.Multipart (MultipartForm, Mem, MultipartData)
```

**Step 2: Add importTransactions to TransactionsAPI in src/Api.hs**

Change:

```haskell
data TransactionsAPI mode = TransactionsAPI
  { listTransactions :: mode :- ...
  , getTransaction   :: mode :- ...
  , createTransaction :: mode :- ...
  } deriving Generic
```

To:

```haskell
data TransactionsAPI mode = TransactionsAPI
  { listTransactions :: mode :- ...
  , getTransaction   :: mode :- ...
  , createTransaction :: mode :- ...
  , importTransactions :: mode :-
      "import" :>
      Capture "rules" Text :>
      MultipartForm Mem (MultipartData Mem) :>
      Post '[JSON] ImportResponse
  } deriving Generic
```

**Step 3: Export toTransactionJSON and toPostingJSON from src/Api/Transactions.hs**

Change the module header from:

```haskell
module Api.Transactions
  ( transactionsHandlers
  ) where
```

To:

```haskell
module Api.Transactions
  ( transactionsHandlers
  , toTransactionJSON
  , toPostingJSON
  ) where
```

**Step 4: Add a placeholder handler in transactionsHandlers**

In the `transactionsHandlers` record in `src/Api/Transactions.hs`, add:

```haskell
transactionsHandlers :: TransactionsAPI (AsServerT AppM)
transactionsHandlers = TransactionsAPI
  { listTransactions   = handleListTransactions
  , getTransaction     = handleGetTransaction
  , createTransaction  = handleCreateTransaction
  , importTransactions = \_ _ -> throwError err501
  }
```

The placeholder returns 501 Not Implemented — it will be replaced in Task 6.

**Step 5: Build to confirm it compiles**

```bash
stack build --fast 2>&1
```

Expected: no errors (the placeholder is enough to satisfy the type checker).

**Step 6: Commit**

```bash
git add src/Api.hs src/Api/Transactions.hs
git commit -m "feat: add importTransactions route to TransactionsAPI"
```

---

### Task 5: Implement the import handler

**Files:**
- Create: `src/Api/Import.hs`

**Step 1: Create src/Api/Import.hs**

```haskell
module Api.Import
  ( handleImportTransactions
  ) where

import Control.Exception (bracket, try, SomeException)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Except (runExceptT)
import Data.List (partition, sort)
import Data.Set (Set)
import Data.Text (Text)
import Servant
import Servant.Multipart
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)

import qualified Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Hledger as H

import Api.Convert (mixedAmountToJSON, toStatusJSON)
import Api.Transactions (toTransactionJSON)
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
    []    -> throwError err400 { errBody = "Missing 'file' field in multipart upload" }
    (f:_) -> pure f
  let csvBytes = fdPayload fd

  -- 2. Resolve and validate rules file
  rulesDir <- asks (configRulesDir . envConfig)
  let rulesPath = rulesDir </> T.unpack rulesName <> ".csv.rules"
  rulesExists <- liftIO $ doesFileExist rulesPath
  unless rulesExists $
    throwError err400
      { errBody = "Rules file not found: " <> LBS.fromStrict (TE.encodeUtf8 (T.pack rulesPath)) }

  -- 3. Write CSV bytes to a temp file, parse with hledger, then clean up
  let opts = H.definputopts { H.mrules_file_ = Just rulesPath }
  parseResult <- liftIO $ withTempCsvFile csvBytes $ \csvPath ->
    runExceptT $ H.readJournalFile opts csvPath

  csvJournal <- case parseResult of
    Left err -> throwError err400
      { errBody = "CSV parse error: " <> Data.Aeson.encode (show err) }
    Right j  -> pure j

  -- 4. Deduplicate: skip any transaction already in the journal
  journal <- getJournal
  let existingFps = Set.fromList $ map txnFingerprint (H.jtxns journal)
      (toImport, skipped) = partition
        (\t -> txnFingerprint t `Set.notMember` existingFps)
        (H.jtxns csvJournal)

  -- 5. Append new transactions to journal file
  journalPath <- asks (configJournalPath . envConfig)
  let txnTexts = concatMap (\t -> "\n" <> H.showTransaction t) toImport
  writeResult <- liftIO $ try @SomeException $
    appendFile journalPath (T.unpack txnTexts)
  case writeResult of
    Left ex ->
      throwError err500 { errBody = "Failed to write to journal: " <> Data.Aeson.encode (show ex) }
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
txnFingerprint :: H.Transaction -> (H.Day, Text, [(Text, Text)])
txnFingerprint t =
  ( H.tdate t
  , H.tdescription t
  , sort [(H.paccount p, T.pack $ show $ H.pamount p) | p <- H.tpostings t]
  )
```

**Step 2: Add `catch` import — it is used in withTempCsvFile**

`catch` is from `Control.Exception`, which is already imported via `try`. Confirm the import line is:

```haskell
import Control.Exception (bracket, try, SomeException, catch)
```

**Step 3: Build to confirm it compiles**

```bash
stack build --fast 2>&1
```

Expected: no errors.

**Step 4: Commit**

```bash
git add src/Api/Import.hs
git commit -m "feat: implement handleImportTransactions"
```

---

### Task 6: Wire up the handler

**Files:**
- Modify: `src/Api/Transactions.hs`
- Modify: `src/Lib.hs`

**Step 1: Replace placeholder in transactionsHandlers**

In `src/Api/Transactions.hs`, replace the placeholder:

```haskell
, importTransactions = \_ _ -> throwError err501
```

with:

```haskell
, importTransactions = handleImportTransactions
```

And add the import at the top:

```haskell
import Api.Import (handleImportTransactions)
```

**Step 2: Add servant-multipart context to Lib.hs**

`servant-multipart` requires a `MultipartOptions` in the Servant context. In `src/Lib.hs`, update the `mkApp` function:

```haskell
import Servant.Multipart (defaultMultipartOptions, generalizeMultipartOptions)

mkApp :: AppEnv -> Application
mkApp env = cors (const $ Just corsPolicy) $
  serveWithContext
    (Proxy :: Proxy API)
    (defaultMultipartOptions (Proxy :: Proxy Mem) :. EmptyContext)
    (hoistServerWithContext
      (Proxy :: Proxy API)
      (Proxy :: Proxy '[MultipartOptions Mem])
      (nt env)
      server)
  where
    nt :: AppEnv -> AppM a -> Handler a
    nt e action = Handler $ ExceptT $ try (runAppM e action)
```

Also update the import in `src/Lib.hs`:

```haskell
import Servant.Multipart (MultipartOptions, Mem, defaultMultipartOptions)
```

**Step 3: Build to confirm it compiles**

```bash
stack build --fast 2>&1
```

Expected: no errors.

**Step 4: Smoke test**

Run the server and test with curl:

```bash
# Start server
stack run -- --rules-dir /path/to/rules

# Test with a sample CSV (replace with your rules file name)
curl -X POST http://localhost:8080/api/v1/transactions/import/mybank \
  -F "file=@/path/to/export.csv" \
  -v
```

Expected: JSON response with `imported`, `skipped`, `transactions`.

Test the 400 case (missing rules file):

```bash
curl -X POST http://localhost:8080/api/v1/transactions/import/nonexistent \
  -F "file=@/path/to/export.csv" \
  -v
```

Expected: HTTP 400 with "Rules file not found" message.

**Step 5: Commit**

```bash
git add src/Api/Transactions.hs src/Lib.hs
git commit -m "feat: wire up csv import handler"
```
