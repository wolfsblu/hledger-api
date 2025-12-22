{-# LANGUAGE OverloadedStrings #-}

module Api.Transactions
  ( transactionsHandlers
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, UTCTime(..), getCurrentTime)
import Servant
import Servant.Server.Generic (AsServerT)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Hledger as H

import Api (TransactionsAPI(..))
import Api.Types
import App (AppM, getJournal)

-- | Handlers for the transactions API
transactionsHandlers :: TransactionsAPI (AsServerT AppM)
transactionsHandlers = TransactionsAPI
  { listTransactions = handleListTransactions
  , getTransaction   = handleGetTransaction
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
  today <- liftIO $ utctDay <$> getCurrentTime
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

-- | Convert hledger Transaction to JSON format
toTransactionJSON :: Int -> H.Transaction -> TransactionJSON
toTransactionJSON idx txn = TransactionJSON
  { txnIndex       = idx
  , txnDate        = H.tdate txn
  , txnDate2       = H.tdate2 txn
  , txnStatus      = toStatusJSON $ H.tstatus txn
  , txnCode        = H.tcode txn
  , txnDescription = H.tdescription txn
  , txnComment     = H.tcomment txn
  , txnTags        = H.ttags txn
  , txnPostings    = map toPostingJSON $ H.tpostings txn
  }

-- | Convert posting to JSON format
toPostingJSON :: H.Posting -> PostingJSON
toPostingJSON p = PostingJSON
  { postingAccount = H.paccount p
  , postingAmount  = mixedAmountToJSON $ H.pamount p
  , postingStatus  = toStatusJSON $ H.pstatus p
  }

-- | Convert hledger Status to JSON format
toStatusJSON :: H.Status -> StatusJSON
toStatusJSON H.Unmarked = Unmarked
toStatusJSON H.Pending  = Pending
toStatusJSON H.Cleared  = Cleared

-- | Convert MixedAmount to JSON format
mixedAmountToJSON :: H.MixedAmount -> MixedAmountJSON
mixedAmountToJSON ma =
  MixedAmountJSON $ map amountToJSON $ H.amounts ma

-- | Convert Amount to JSON format
amountToJSON :: H.Amount -> AmountJSON
amountToJSON a = AmountJSON
  { amountQuantity  = fromRational (toRational (H.aquantity a))
  , amountCommodity = H.acommodity a
  }
