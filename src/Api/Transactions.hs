{-# LANGUAGE TypeApplications #-}

module Api.Transactions
  ( transactionsHandlers
  , applyFilters
  , applySorting
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (encode)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Scientific (Scientific, fromFloatDigits)
import Data.Text (Text)
import Data.Time (Day)
import Servant
import Servant.Server.Generic (AsServerT)

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Hledger as H

import Api (TransactionsAPI(..))
import Api.Convert
import Api.Import (handleImportTransactions, handleListImportRules)
import Api.Types
import App (AppM, AppEnv(..), AppConfig(..), getJournal, modifyJournal)

-- | Handlers for the transactions API
transactionsHandlers :: TransactionsAPI (AsServerT AppM)
transactionsHandlers = TransactionsAPI
  { listTransactions        = handleListTransactions
  , getTransaction          = handleGetTransaction
  , createTransaction       = handleCreateTransaction
  , listImportRules         = handleListImportRules
  , importTransactions      = handleImportTransactions
  , bulkCreateTransactions  = handleBulkCreateTransactions
  }

-- | List transactions with optional filters
handleListTransactions
  :: Maybe Day        -- ^ from date
  -> Maybe Day        -- ^ to date
  -> [Text]           -- ^ account filters (OR)
  -> Maybe Text       -- ^ description filter
  -> [Text]           -- ^ status filters (OR)
  -> [Text]           -- ^ tag filters (OR)
  -> Maybe Double     -- ^ minAmount
  -> Maybe Double     -- ^ maxAmount
  -> Maybe Text       -- ^ sort spec
  -> Maybe Int        -- ^ limit
  -> Maybe Int        -- ^ offset
  -> AppM (PaginatedResponse TransactionJSON)
handleListTransactions mFrom mTo accounts mDesc statuses tags mMinAmt mMaxAmt mSort mLimit mOffset = do
  -- Parse and validate statuses
  parsedStatuses <- mapM parseStatus statuses

  -- Parse and validate sort spec
  sortSpecs <- case mSort of
    Nothing -> pure [SortSpec SortDate Asc]
    Just s  -> case parseSortParam s of
      Left err    -> throwError err400 { errBody = encode err }
      Right specs -> pure specs

  journal <- getJournal
  let limit = fromMaybe 100 mLimit
      offset = fromMaybe 0 mOffset
      txns = H.jtxns journal
      minAmt = fmap fromFloatDigits mMinAmt
      maxAmt = fmap fromFloatDigits mMaxAmt
      filtered = applyFilters mFrom mTo accounts mDesc parsedStatuses tags minAmt maxAmt txns
      sorted = applySorting sortSpecs filtered
      total = length sorted
      paged = take limit $ drop offset sorted
      indexed = zipWith toTransactionJSON [offset..] paged
  return PaginatedResponse
    { pageData   = indexed
    , pageTotal  = total
    , pageLimit  = limit
    , pageOffset = offset
    }

-- | Get a single transaction by index
handleGetTransaction :: Int -> AppM TransactionDetail
handleGetTransaction idx = do
  journal <- getJournal
  let txns = sortBy (comparing H.tdate) $ H.jtxns journal
  if idx >= 0 && idx < length txns
    then return $ toTransactionJSON idx (txns !! idx)
    else throwError err404 { errBody = "Transaction not found" }

-- | Parse and validate a status text value
parseStatus :: Text -> AppM H.Status
parseStatus s = case T.toLower s of
  "unmarked" -> pure H.Unmarked
  "pending"  -> pure H.Pending
  "cleared"  -> pure H.Cleared
  _          -> throwError err400 { errBody = "Invalid status: must be unmarked, pending, or cleared" }

-- | Apply filters to transaction list
applyFilters
  :: Maybe Day -> Maybe Day -> [Text] -> Maybe Text
  -> [H.Status] -> [Text] -> Maybe Scientific -> Maybe Scientific
  -> [H.Transaction] -> [H.Transaction]
applyFilters mFrom mTo accounts mDesc statuses tags mMinAmt mMaxAmt = filter matches
  where
    matches txn = all ($ txn)
      [ maybe (const True) (\d t -> H.tdate t >= d) mFrom
      , maybe (const True) (\d t -> H.tdate t <= d) mTo
      , matchesAccounts accounts
      , maybe (const True) matchesDesc mDesc
      , matchesStatuses statuses
      , matchesTags tags
      , maybe (const True) matchesMinAmt mMinAmt
      , maybe (const True) matchesMaxAmt mMaxAmt
      ]

    matchesAccounts [] _ = True
    matchesAccounts accts txn =
      any (\acct -> any (\p -> acct `T.isInfixOf` H.paccount p) (H.tpostings txn)) accts

    matchesDesc desc txn =
      T.toLower desc `T.isInfixOf` T.toLower (H.tdescription txn)

    matchesStatuses [] _ = True
    matchesStatuses ss txn = H.tstatus txn `elem` ss

    matchesTags [] _ = True
    matchesTags ts txn = any (`matchesTag` txn) ts

    matchesTag tag txn =
      let (name, rest) = T.breakOn ":" tag
          txnTags = H.ttags txn
      in if T.null rest
        then any (\(n, _) -> T.toLower n == T.toLower name) txnTags
        else let value = T.drop 1 rest  -- drop the ':'
             in any (\(n, v) -> T.toLower n == T.toLower name
                             && T.toLower v == T.toLower value) txnTags

    matchesMinAmt threshold txn =
      any (anyAmountGe threshold) (H.tpostings txn)

    matchesMaxAmt threshold txn =
      any (anyAmountLe threshold) (H.tpostings txn)

    anyAmountGe threshold p =
      any (\a -> toScientific (H.aquantity a) >= threshold) (H.amounts (H.pamount p))

    anyAmountLe threshold p =
      any (\a -> toScientific (H.aquantity a) <= threshold) (H.amounts (H.pamount p))

-- | Apply multi-field sorting to transactions.
-- Uses stable sort (merge sort) applied from least-significant field first.
applySorting :: [SortSpec] -> [H.Transaction] -> [H.Transaction]
applySorting specs txns = foldl' applyOne txns (reverse specs)
  where
    applyOne ts (SortSpec field dir) =
      let cmp = case dir of
            Asc  -> comparing (sortKey field)
            Desc -> flip (comparing (sortKey field))
      in sortBy cmp ts

    sortKey :: SortField -> H.Transaction -> SortKey
    sortKey SortDate        txn = SKDay (H.tdate txn)
    sortKey SortDescription txn = SKText (T.toLower (H.tdescription txn))
    sortKey SortStatus      txn = SKInt (statusOrd (H.tstatus txn))
    sortKey SortAmount      txn = SKScientific (maxAbsAmount txn)

    statusOrd :: H.Status -> Int
    statusOrd H.Unmarked = 0
    statusOrd H.Pending  = 1
    statusOrd H.Cleared  = 2

    maxAbsAmount :: H.Transaction -> Scientific
    maxAbsAmount txn =
      let amounts = concatMap (H.amounts . H.pamount) (H.tpostings txn)
          absAmounts = map (abs . toScientific . H.aquantity) amounts
      in case absAmounts of
        [] -> 0
        xs -> maximum xs

-- | Wrapper for sort keys supporting heterogeneous comparison
data SortKey = SKDay Day | SKText Text | SKInt Int | SKScientific Scientific
  deriving (Eq, Ord)

-- | Convert hledger's Decimal quantity to Scientific
toScientific :: H.Quantity -> Scientific
toScientific = fromFloatDigits . (realToFrac :: H.Quantity -> Double)

-- | Validate and balance a single CreateTransactionRequest.
-- Returns either an error message or the balanced hledger Transaction.
validateAndBalance :: CreateTransactionRequest -> Either String H.Transaction
validateAndBalance req = do
  let postings = ctPostings req
  if length postings < 2
    then Left "A transaction must have at least 2 postings"
    else Right ()
  let missingCount = length $ filter (\p -> cpAmount p == Nothing) postings
  if missingCount > 1
    then Left "At most one posting may omit its amount"
    else Right ()
  if missingCount == 0
    then do
      let allAmounts = concatMap (unMixedAmount . fromMaybe (MixedAmountJSON []) . cpAmount) postings
          byCommodity :: Map Text Double
          byCommodity = Map.fromListWith (+)
            [ (amountCommodity a, realToFrac (amountQuantity a)) | a <- allAmounts ]
          unbalanced = Map.filter (\v -> abs v > 1e-9) byCommodity
      if Map.null unbalanced
        then Right ()
        else Left $ "Postings do not balance. Unbalanced commodities: "
                 <> show (Map.keys unbalanced)
    else Right ()
  let opts = H.defbalancingopts{H.infer_balancing_costs_ = True}
  case H.balanceTransaction opts (fromCreateTransaction req) of
    Left err -> Left $ "Cannot balance transaction: " <> show err
    Right t  -> Right t

-- | Create a new transaction and append it to the journal file
handleCreateTransaction
  :: CreateTransactionRequest
  -> AppM (Headers '[Header "Location" Text] TransactionJSON)
handleCreateTransaction req = do
  balancedTxn <- case validateAndBalance req of
    Left msg -> throwError err400 { errBody = encode msg }
    Right t  -> pure t

  journal <- getJournal
  let idx = length (H.jtxns journal)
      txn = balancedTxn { H.tindex = fromIntegral idx + 1 }

  journalPath <- asks (configJournalPath . envConfig)
  let txnText = "\n" <> H.showTransaction txn
  writeResult <- liftIO $ try @SomeException $
    appendFile journalPath (T.unpack txnText)
  case writeResult of
    Left ex ->
      throwError err500 { errBody = "Failed to write to journal: " <> encode (show ex) }
    Right () -> pure ()

  modifyJournal (\j -> j { H.jtxns = H.jtxns j ++ [txn] })

  let result   = toTransactionJSON idx txn
      location = "/api/v1/transactions/" <> T.pack (show idx)
  return $ addHeader location result

-- | Create multiple transactions atomically.
-- Validates all before writing any; on failure returns 400 with the
-- 0-based index of the first offending transaction.
handleBulkCreateTransactions
  :: [CreateTransactionRequest]
  -> AppM [TransactionJSON]
handleBulkCreateTransactions [] = return []
handleBulkCreateTransactions reqs = do
  -- 1. VALIDATE ALL
  balancedTxns <- case validateAll (zip [0..] reqs) of
    Left (i, msg) ->
      throwError err400
        { errBody = "Transaction " <> encode (show (i :: Int)) <> ": " <> encode msg }
    Right ts -> pure ts

  -- 2. ASSIGN INDICES
  journal <- getJournal
  let baseIdx = length (H.jtxns journal)
      indexed = zipWith (\i t -> t { H.tindex = fromIntegral (baseIdx + i) + 1 })
                  [0..] balancedTxns

  -- 3. WRITE ALL ATOMICALLY (single appendFile call)
  journalPath <- asks (configJournalPath . envConfig)
  let txnText = concatMap (\t -> T.unpack $ "\n" <> H.showTransaction t) indexed
  writeResult <- liftIO $ try @SomeException $
    appendFile journalPath txnText
  case writeResult of
    Left ex ->
      throwError err500 { errBody = "Failed to write to journal: " <> encode (show ex) }
    Right () -> pure ()

  modifyJournal (\j -> j { H.jtxns = H.jtxns j ++ indexed })

  return $ zipWith toTransactionJSON [baseIdx..] indexed

  where
    validateAll [] = Right []
    validateAll ((i, req) : rest) =
      case validateAndBalance req of
        Left msg -> Left (i, msg)
        Right t  -> fmap (t :) (validateAll rest)
