# CSV Import API Design

## Endpoint

```
POST /api/v1/transactions/import/{rules}
  Content-Type: multipart/form-data
  field "file": the CSV file
```

- `{rules}` — name of a rules file pre-configured on the server (e.g. `bank-checking` maps to `{configRulesDir}/bank-checking.csv.rules`)
- Returns `400 Bad Request` if the rules file does not exist

## Configuration

`AppConfig` gains one new field:

```haskell
data AppConfig = AppConfig
  { configJournalPath :: FilePath
  , configRulesDir    :: FilePath   -- directory containing .csv.rules files
  , configPort        :: Int
  , configHost        :: String
  }
```

## New Dependency

`servant-multipart` — required for multipart form upload support.

## Response Type

```haskell
data ImportResponse = ImportResponse
  { importedCount :: Int
  , skippedCount  :: Int
  , importedTxns  :: [TransactionJSON]
  } deriving (Generic, ToJSON, ToSchema)
```

## Implementation Flow

1. **Extract CSV bytes** from the multipart upload (400 if no `file` field)
2. **Resolve rules file** — look up `{configRulesDir}/{rules}.csv.rules` (400 if not found)
3. **Parse CSV** — write CSV bytes to a temp file, call hledger-lib's CSV reader with the rules file to produce `[Transaction]` (400 on parse error)
4. **Deduplicate** — compare parsed transactions against the current journal using hledger's comparison logic; split into `toImport` and `skipped`
5. **Write to disk** — append `toImport` transactions to the journal file
6. **Update in-memory journal** — call `modifyJournal` to append the new transactions
7. **Respond** — return `ImportResponse { importedCount, skippedCount, importedTxns }`

Deduplication is always on. Any transaction already present in the journal is silently skipped.

## New Module

`src/Api/Import.hs` — mirrors the structure of `src/Api/Transactions.hs`.

The `TransactionsAPI` type in `src/Api.hs` gains the new route:

```haskell
, importTransactions :: mode :-
    "import" :>
    Capture "rules" Text :>
    MultipartForm Mem (MultipartData Mem) :>
    Post '[JSON] ImportResponse
```
