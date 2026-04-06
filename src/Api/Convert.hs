-- | Conversion utilities for transforming hledger types to API JSON types
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
    -- * Transaction conversions
  , toTransactionJSON
  , toPostingJSON
    -- * Hledger helpers
  , ledgerAccountBalance
  , toStatusJSON
  ) where

import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Hledger as H

import Api.Types

-- | Convert hledger Amount to JSON format
amountToJSON :: H.Amount -> AmountJSON
amountToJSON a = AmountJSON
  { amountQuantity  = fromRational (toRational (H.aquantity a))
  , amountCommodity = H.acommodity a
  }

-- | Convert hledger MixedAmount to JSON format
mixedAmountToJSON :: H.MixedAmount -> MixedAmountJSON
mixedAmountToJSON ma = MixedAmountJSON $ map amountToJSON $ H.amounts ma

-- | Get the balance of an account in a ledger
--
-- In newer hledger-lib versions (>= 1.43), laccounts is a list rather than a Map,
-- so we need to search for the account by name.
ledgerAccountBalance :: H.Ledger -> H.AccountName -> H.MixedAmount
ledgerAccountBalance ledger name =
  case find (\acct -> H.aname acct == name) (H.laccounts ledger) of
    Just acct -> H.aibalance acct
    Nothing   -> mempty

-- | Convert hledger Status to JSON format
toStatusJSON :: H.Status -> StatusJSON
toStatusJSON H.Unmarked = Unmarked
toStatusJSON H.Pending  = Pending
toStatusJSON H.Cleared  = Cleared

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

-- | Convert hledger Posting to JSON format
toPostingJSON :: H.Posting -> PostingJSON
toPostingJSON p = PostingJSON
  { postingAccount = H.paccount p
  , postingAmount  = mixedAmountToJSON $ H.pamount p
  , postingStatus  = toStatusJSON $ H.pstatus p
  }
