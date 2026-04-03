# Create Transaction API Design

**Date:** 2026-04-03

## Goal

Add a `POST /api/v1/transactions` endpoint that appends a new transaction to the hledger journal file and returns the created transaction.

## Decisions

- **Request format:** JSON (`CreateTransactionRequest`)
- **Balance validation:** Allow exactly one posting to omit its amount (hledger auto-balance); if all postings have amounts, validate they sum to zero per commodity
- **Write strategy:** Validate in memory → append to disk → update in-memory `IORef` only on success
- **Response:** `201 Created` with `TransactionJSON` body and `Location: /api/v1/transactions/{index}` header

---

## Section 1: New Input Types (`src/Api/Types/Transaction.hs`)

```haskell
data CreatePostingRequest = CreatePostingRequest
  { cpAccount :: Text
  , cpAmount  :: Maybe MixedAmountJSON  -- omit for auto-balance
  , cpStatus  :: Maybe StatusJSON       -- defaults to Unmarked
  }

data CreateTransactionRequest = CreateTransactionRequest
  { ctDate        :: Day
  , ctDate2       :: Maybe Day
  , ctStatus      :: Maybe StatusJSON   -- defaults to Unmarked
  , ctCode        :: Maybe Text
  , ctDescription :: Text
  , ctComment     :: Maybe Text
  , ctPostings    :: [CreatePostingRequest]  -- min 2
  }
```

Both get `FromJSON` + `ToSchema` instances. `MixedAmountJSON` and `StatusJSON` already have `FromJSON`.

---

## Section 2: API Endpoint (`src/Api.hs`)

Add `createTransaction` to `TransactionsAPI`:

```haskell
data TransactionsAPI mode = TransactionsAPI
  { listTransactions  :: mode :- ...  -- existing
  , getTransaction    :: mode :- ...  -- existing
  , createTransaction :: mode :-
      ReqBody '[JSON] CreateTransactionRequest :>
      PostCreated '[JSON] (Headers '[Header "Location" Text] TransactionJSON)
  } deriving Generic
```

`PostCreated` is Servant's alias for `Verb 'POST 201`.

---

## Section 3: Handler Logic (`src/Api/Transactions.hs`)

```
1. VALIDATE
   - Require ≥ 2 postings → 400 if not
   - Count postings with no amount:
     - 0 missing: verify sum per commodity = 0 → 400 if unbalanced
     - 1 missing: allowed (hledger infers it)
     - 2+ missing: 400

2. CONVERT
   - CreateTransactionRequest → H.Transaction (via Convert.hs helpers)
   - Use hledger's autoBalanceTransaction to fill in the missing posting amount

3. WRITE TO DISK
   - Format transaction as journal text via hledger's showTransaction
   - Append to journal file (with a leading newline separator)
   - On IO failure → 500, IORef is untouched

4. UPDATE MEMORY
   - modifyIORef to append the transaction to journal's jtxns list
   - Assign index = length jtxns before append

5. RESPOND
   - 201 Created
   - Body: TransactionJSON with assigned index
   - Header: Location: /api/v1/transactions/{index}
```

---

## Section 4: Conversion Helpers (`src/Api/Convert.hs`)

New reverse-direction helpers:

```haskell
fromStatusJSON      :: StatusJSON -> H.Status
fromAmountJSON      :: AmountJSON -> H.Amount
fromMixedAmountJSON :: MixedAmountJSON -> H.MixedAmount
fromCreatePosting   :: CreatePostingRequest -> H.Posting
fromCreateTransaction :: CreateTransactionRequest -> H.Transaction
```

`fromCreateTransaction` sets `tindex=0` and a dummy `sourcepos`. The API index is the 0-based position in `jtxns`, not hledger's internal `tindex`.

---

## Section 5: App.hs Addition

```haskell
modifyJournal :: (Journal -> Journal) -> AppM ()
modifyJournal f = do
  ref <- asks envJournal
  liftIO $ modifyIORef ref f
```

Called after successful disk write:

```haskell
modifyJournal (\j -> j { H.jtxns = H.jtxns j ++ [newTxn] })
```

---

## Files Touched

| File | Change |
|---|---|
| `src/Api/Types/Transaction.hs` | Add `CreatePostingRequest`, `CreateTransactionRequest` |
| `src/Api.hs` | Add `createTransaction` to `TransactionsAPI` |
| `src/Api/Transactions.hs` | Add `handleCreateTransaction` |
| `src/Api/Convert.hs` | Add `from*` conversion helpers |
| `src/App.hs` | Add `modifyJournal` |
