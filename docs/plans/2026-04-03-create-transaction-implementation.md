# Create Transaction API Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add `POST /api/v1/transactions` to append a new transaction to the hledger journal file and return the created transaction.

**Architecture:** Validate in memory → append formatted text to disk → update in-memory `IORef Journal` only on success. Returns `201 Created` with `TransactionJSON` body and `Location` header.

**Tech Stack:** Haskell, Servant, hledger-lib (`import qualified Hledger as H`), Stack (LTS 24.25)

**Design doc:** `docs/plans/2026-04-03-create-transaction-api-design.md`

---

## Task 1: Add input types to `src/Api/Types/Transaction.hs`

**Files:**
- Modify: `src/Api/Types/Transaction.hs`

Add `CreatePostingRequest` and `CreateTransactionRequest` after the existing types. Both need `FromJSON` and `ToSchema` instances. `MixedAmountJSON` and `StatusJSON` already have `FromJSON` so they compose directly.

**Step 1: Add imports**

At the top of `src/Api/Types/Transaction.hs`, add to the existing import block:

```haskell
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), (.:?), object, withObject)
```

Replace the existing `Data.Aeson` import line (currently `import Data.Aeson (ToJSON(..), (.=), object)`).

**Step 2: Add the new types and instances**

Append to the bottom of `src/Api/Types/Transaction.hs`:

```haskell
-- | Posting within a create-transaction request
data CreatePostingRequest = CreatePostingRequest
  { cpAccount :: Text
  , cpAmount  :: Maybe MixedAmountJSON
  , cpStatus  :: Maybe StatusJSON
  } deriving (Show, Eq, Generic)

instance FromJSON CreatePostingRequest where
  parseJSON = withObject "CreatePostingRequest" $ \v ->
    CreatePostingRequest
      <$> v .:  "account"
      <*> v .:? "amount"
      <*> v .:? "status"

instance ToSchema CreatePostingRequest where
  declareNamedSchema _ = do
    mixedRef <- declareSchemaRef (Proxy :: Proxy MixedAmountJSON)
    statusRef <- declareSchemaRef (Proxy :: Proxy StatusJSON)
    return $ NamedSchema (Just "CreatePosting") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~ IOHM.fromList
          [ ("account", Inline $ mempty & type_ ?~ OpenApiString)
          , ("amount",  mixedRef)
          , ("status",  statusRef)
          ]
      & required .~ ["account"]

-- | Request body for creating a new transaction
data CreateTransactionRequest = CreateTransactionRequest
  { ctDate        :: Day
  , ctDate2       :: Maybe Day
  , ctStatus      :: Maybe StatusJSON
  , ctCode        :: Maybe Text
  , ctDescription :: Text
  , ctComment     :: Maybe Text
  , ctPostings    :: [CreatePostingRequest]
  } deriving (Show, Eq, Generic)

instance FromJSON CreateTransactionRequest where
  parseJSON = withObject "CreateTransactionRequest" $ \v ->
    CreateTransactionRequest
      <$> v .:  "date"
      <*> v .:? "date2"
      <*> v .:? "status"
      <*> v .:? "code"
      <*> v .:  "description"
      <*> v .:? "comment"
      <*> v .:  "postings"

instance ToSchema CreateTransactionRequest where
  declareNamedSchema _ = do
    statusRef  <- declareSchemaRef (Proxy :: Proxy StatusJSON)
    postingRef <- declareSchemaRef (Proxy :: Proxy CreatePostingRequest)
    return $ NamedSchema (Just "CreateTransactionRequest") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~ IOHM.fromList
          [ ("date",        Inline $ mempty & type_ ?~ OpenApiString & format ?~ "date")
          , ("date2",       Inline $ mempty & type_ ?~ OpenApiString & format ?~ "date")
          , ("status",      statusRef)
          , ("code",        Inline $ mempty & type_ ?~ OpenApiString)
          , ("description", Inline $ mempty & type_ ?~ OpenApiString)
          , ("comment",     Inline $ mempty & type_ ?~ OpenApiString)
          , ("postings",    Inline $ mempty & type_ ?~ OpenApiArray
                                            & items ?~ OpenApiItemsObject postingRef)
          ]
      & required .~ ["date", "description", "postings"]
```

**Step 3: Export the new types from the module header**

Update the module export list in `src/Api/Types/Transaction.hs`:

```haskell
module Api.Types.Transaction
  ( TransactionJSON(..)
  , PostingJSON(..)
  , TransactionDetail
  , CreatePostingRequest(..)
  , CreateTransactionRequest(..)
  ) where
```

**Step 4: Build to verify**

```bash
stack build --fast 2>&1
```

Expected: compiles successfully (or only errors about unused imports, which can be fixed).

**Step 5: Commit**

```bash
git add src/Api/Types/Transaction.hs
git commit -m "feat: add CreateTransactionRequest and CreatePostingRequest types"
```

---

## Task 2: Add `createTransaction` endpoint to `src/Api.hs`

**Files:**
- Modify: `src/Api.hs`

**Step 1: Add Servant imports for 201 and headers**

`PostCreated` and `Headers`/`Header` are already in `Servant` — no new imports needed.

**Step 2: Add the route to `TransactionsAPI`**

In `src/Api.hs`, update `TransactionsAPI` from:

```haskell
data TransactionsAPI mode = TransactionsAPI
  { listTransactions :: mode :-
      ...
  , getTransaction :: mode :-
      ...
  } deriving Generic
```

to:

```haskell
data TransactionsAPI mode = TransactionsAPI
  { listTransactions :: mode :-
      QueryParam "from" Day :>
      QueryParam "to" Day :>
      QueryParam "account" Text :>
      QueryParam "description" Text :>
      QueryParam "limit" Int :>
      QueryParam "offset" Int :>
      Get '[JSON] (PaginatedResponse TransactionJSON)

  , getTransaction :: mode :-
      Capture "index" Int :>
      Get '[JSON] TransactionDetail

  , createTransaction :: mode :-
      ReqBody '[JSON] CreateTransactionRequest :>
      PostCreated '[JSON] (Headers '[Header "Location" Text] TransactionJSON)
  } deriving Generic
```

**Step 3: Build to verify (expect a type error about missing handler — that's fine)**

```bash
stack build --fast 2>&1
```

Expected: error mentioning `createTransaction` field missing in `transactionsHandlers` — this confirms the type is wired in.

**Step 4: Commit**

```bash
git add src/Api.hs
git commit -m "feat: add createTransaction endpoint to TransactionsAPI"
```

---

## Task 3: Add `modifyJournal` to `src/App.hs`

**Files:**
- Modify: `src/App.hs`

**Step 1: Add `modifyIORef` to imports**

Update the `Data.IORef` import:

```haskell
import Data.IORef (IORef, readIORef, modifyIORef)
```

**Step 2: Add the function**

Append to `src/App.hs` (after `getJournal`):

```haskell
-- | Modify the journal in the environment
modifyJournal :: (Journal -> Journal) -> AppM ()
modifyJournal f = do
  ref <- asks envJournal
  liftIO $ modifyIORef ref f
```

**Step 3: Export it from the module header**

Update the module export list:

```haskell
module App
  ( AppConfig(..)
  , AppEnv(..)
  , AppM(..)
  , runAppM
  , getJournal
  , modifyJournal
  , defaultConfig
  ) where
```

**Step 4: Build to verify**

```bash
stack build --fast 2>&1
```

Expected: same error as before (missing `createTransaction` handler) — no new errors.

**Step 5: Commit**

```bash
git add src/App.hs
git commit -m "feat: add modifyJournal helper to App"
```

---

## Task 4: Add conversion helpers to `src/Api/Convert.hs`

**Files:**
- Modify: `src/Api/Convert.hs`

These helpers convert our JSON input types into hledger's internal types.

**Step 1: Add imports**

Add to the import block in `src/Api/Convert.hs`:

```haskell
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (Day)
```

**Step 2: Add the reverse-direction converters**

Append to `src/Api/Convert.hs`:

```haskell
-- | Convert JSON Status to hledger Status
fromStatusJSON :: StatusJSON -> H.Status
fromStatusJSON Unmarked = H.Unmarked
fromStatusJSON Pending  = H.Pending
fromStatusJSON Cleared  = H.Cleared

-- | Convert JSON Amount to hledger Amount
fromAmountJSON :: AmountJSON -> H.Amount
fromAmountJSON a = H.nullamt
  { H.acommodity = amountCommodity a
  , H.aquantity  = fromRational (toRational (amountQuantity a))
  }

-- | Convert JSON MixedAmount to hledger MixedAmount
fromMixedAmountJSON :: MixedAmountJSON -> H.MixedAmount
fromMixedAmountJSON (MixedAmountJSON amts) =
  H.mixed (map fromAmountJSON amts)

-- | Convert CreatePostingRequest to hledger Posting
-- If cpAmount is Nothing, uses missingmixedamt so hledger can auto-balance
fromCreatePosting :: CreatePostingRequest -> H.Posting
fromCreatePosting p = H.nullposting
  { H.paccount = cpAccount p
  , H.pamount  = maybe H.missingmixedamt fromMixedAmountJSON (cpAmount p)
  , H.pstatus  = fromMaybe H.Unmarked (fmap fromStatusJSON (cpStatus p))
  }

-- | Convert CreateTransactionRequest to hledger Transaction
fromCreateTransaction :: CreateTransactionRequest -> H.Transaction
fromCreateTransaction req = H.nulltransaction
  { H.tdate        = ctDate req
  , H.tdate2       = ctDate2 req
  , H.tstatus      = fromMaybe H.Unmarked (fmap fromStatusJSON (ctStatus req))
  , H.tcode        = fromMaybe "" (ctCode req)
  , H.tdescription = ctDescription req
  , H.tcomment     = fromMaybe "" (ctComment req)
  , H.tpostings    = map fromCreatePosting (ctPostings req)
  }
```

**Step 3: Export the new helpers**

Update the module export list in `src/Api/Convert.hs`:

```haskell
module Api.Convert
  ( -- * Amount conversions
    amountToJSON
  , mixedAmountToJSON
    -- * Reverse conversions
  , fromStatusJSON
  , fromAmountJSON
  , fromMixedAmountJSON
  , fromCreatePosting
  , fromCreateTransaction
    -- * Hledger helpers
  , ledgerAccountBalance
  , toStatusJSON
  ) where
```

**Step 4: Build to verify**

```bash
stack build --fast 2>&1
```

Expected: same `createTransaction` missing-handler error — no new errors from Convert.hs.

**Step 5: Commit**

```bash
git add src/Api/Convert.hs
git commit -m "feat: add from* conversion helpers to Api.Convert"
```

---

## Task 5: Implement `handleCreateTransaction` in `src/Api/Transactions.hs`

**Files:**
- Modify: `src/Api/Transactions.hs`

This is the main handler. It validates, writes to disk, updates memory, and responds.

**Step 1: Add imports**

Add to the import block in `src/Api/Transactions.hs`:

```haskell
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import Data.List (nub)
import System.IO (withFile, IOMode(..), hPutStr)

import qualified Data.Scientific as Sci
import qualified Hledger as H

import App (AppM, getJournal, modifyJournal, AppEnv(..), envConfig, configJournalPath)
import Control.Monad.Reader (asks)
```

Note: `liftIO`, `asks` may already be transitively available — add only what's missing after the first build attempt.

**Step 2: Add the handler**

Append to `src/Api/Transactions.hs`:

```haskell
-- | Create a new transaction and append it to the journal file
handleCreateTransaction
  :: CreateTransactionRequest
  -> AppM (Headers '[Header "Location" Text] TransactionJSON)
handleCreateTransaction req = do
  -- 1. VALIDATE
  let postings = ctPostings req
  when (length postings < 2) $
    throwError err400 { errBody = "A transaction must have at least 2 postings" }

  let missingCount = length $ filter (\p -> cpAmount p == Nothing) postings
  when (missingCount > 1) $
    throwError err400 { errBody = "At most one posting may omit its amount" }

  when (missingCount == 0) $ do
    let allAmounts = concatMap (unMixedAmount . fromJust . cpAmount) postings
        byCommodity = Map.fromListWith (+)
          [ (amountCommodity a, amountQuantity a) | a <- allAmounts ]
        unbalanced = Map.filter (/= 0) byCommodity
    unless (Map.null unbalanced) $
      throwError err400
        { errBody = "Postings do not balance. Unbalanced commodities: "
                 <> encode (Map.keys unbalanced)
        }

  -- 2. CONVERT
  let txn = fromCreateTransaction req

  -- 3. WRITE TO DISK
  journalPath <- asks (configJournalPath . envConfig)
  let txnText = "\n" <> H.showTransaction txn
  writeResult <- liftIO $ try @SomeException $
    appendFile journalPath txnText
  case writeResult of
    Left ex ->
      throwError err500 { errBody = "Failed to write to journal: " <> encode (show ex) }
    Right () -> pure ()

  -- 4. UPDATE MEMORY
  journal <- getJournal
  let idx    = length (H.jtxns journal)
      newTxn = txn { H.tindex = idx + 1 }  -- hledger tindex is 1-based internally
  modifyJournal (\j -> j { H.jtxns = H.jtxns j ++ [newTxn] })

  -- 5. RESPOND
  let result   = toTransactionJSON idx newTxn
      location = "/api/v1/transactions/" <> T.pack (show idx)
  return $ addHeader location result
```

**Step 3: Add required imports at the top of the file**

Add these to the import block:

```haskell
import Control.Exception (try, SomeException)
import Control.Monad (when, unless)
import Data.Aeson (encode)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
```

**Step 4: Wire the handler into `transactionsHandlers`**

Update `transactionsHandlers`:

```haskell
transactionsHandlers :: TransactionsAPI (AsServerT AppM)
transactionsHandlers = TransactionsAPI
  { listTransactions  = handleListTransactions
  , getTransaction    = handleGetTransaction
  , createTransaction = handleCreateTransaction
  }
```

**Step 5: Build to verify**

```bash
stack build --fast 2>&1
```

Expected: clean build. Fix any import/type errors that arise — common issues:
- `H.showTransaction` might return `String`, not `Text` — use `T.pack` if needed
- `appendFile` takes `FilePath` and `String` — use `T.unpack txnText` if `txnText` is `Text`
- `unMixedAmount` unwraps the newtype wrapper for `MixedAmountJSON`

**Step 6: Commit**

```bash
git add src/Api/Transactions.hs
git commit -m "feat: implement handleCreateTransaction"
```

---

## Task 6: Smoke test

**Step 1: Start the server**

```bash
stack run
```

**Step 2: Create a balanced transaction**

```bash
curl -s -X POST http://localhost:8080/api/v1/transactions \
  -H "Content-Type: application/json" \
  -d '{
    "date": "2026-04-03",
    "description": "Coffee",
    "postings": [
      { "account": "expenses:food", "amount": [{ "quantity": 5.0, "commodity": "EUR" }] },
      { "account": "assets:cash" }
    ]
  }' | jq .
```

Expected: `201 Created` with the transaction JSON and `Location` header.

**Step 3: Create an unbalanced transaction (should fail)**

```bash
curl -s -o /dev/null -w "%{http_code}" -X POST http://localhost:8080/api/v1/transactions \
  -H "Content-Type: application/json" \
  -d '{
    "date": "2026-04-03",
    "description": "Bad txn",
    "postings": [
      { "account": "expenses:food",  "amount": [{ "quantity": 5.0, "commodity": "EUR" }] },
      { "account": "assets:cash",    "amount": [{ "quantity": 3.0, "commodity": "EUR" }] }
    ]
  }'
```

Expected: `400`

**Step 4: Fetch the created transaction by index**

```bash
curl -s http://localhost:8080/api/v1/transactions/0 | jq .
```

Expected: the transaction you just created.

**Step 5: Verify it was appended to the journal file**

```bash
tail -10 $LEDGER_FILE
```

Expected: the hledger-formatted transaction at the end of the file.

---

## Notes

- `H.nulltransaction`, `H.nullposting`, `H.nullamt`, `H.missingmixedamt` are all in `Hledger` (re-exported from hledger-lib)
- `H.mixed :: [Amount] -> MixedAmount` constructs a MixedAmount from a list
- `H.showTransaction :: Transaction -> String` — wrap with `T.pack` if needed
- If `H.tindex` causes issues (it's the source file line index, not our API index), just leave it as `0` — our API index is the position in `jtxns`
