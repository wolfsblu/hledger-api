{-# LANGUAGE TypeApplications #-}

module Api.Transactions
  ( transactionsHandlers
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (encode)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
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
  :: Maybe Day      -- ^ from date
  -> Maybe Day      -- ^ to date
  -> Maybe Text     -- ^ account filter
  -> Maybe Text     -- ^ description filter
  -> Maybe Int      -- ^ limit
  -> Maybe Int      -- ^ offset
  -> AppM (PaginatedResponse TransactionJSON)
handleListTransactions mFrom mTo mAccount mDesc mLimit mOffset = do
  journal <- getJournal
  let fromDate = mFrom
      toDate = mTo
      limit = fromMaybe 100 mLimit
      offset = fromMaybe 0 mOffset
      txns = H.jtxns journal
      -- Apply filters
      filtered = applyFilters fromDate toDate mAccount mDesc txns
      sorted = sortOn H.tdate filtered
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
  let txns = sortOn H.tdate $ H.jtxns journal
  if idx >= 0 && idx < length txns
    then return $ toTransactionJSON idx (txns !! idx)
    else throwError err404 { errBody = "Transaction not found" }

-- | Apply filters to transaction list
applyFilters
  :: Maybe Day -> Maybe Day -> Maybe Text -> Maybe Text
  -> [H.Transaction] -> [H.Transaction]
applyFilters mFrom mTo mAccount mDesc = filter matches
  where
    matches txn = all ($ txn)
      [ maybe (const True) (\d t -> H.tdate t >= d) mFrom
      , maybe (const True) (\d t -> H.tdate t <= d) mTo
      , maybe (const True) matchesAccount mAccount
      , maybe (const True) matchesDesc mDesc
      ]

    matchesAccount acct txn =
      any (\p -> acct `T.isInfixOf` H.paccount p) (H.tpostings txn)

    matchesDesc desc txn =
      T.toLower desc `T.isInfixOf` T.toLower (H.tdescription txn)

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
