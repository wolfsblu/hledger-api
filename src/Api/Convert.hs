{-# LANGUAGE OverloadedStrings #-}

-- | Conversion utilities for transforming hledger types to API JSON types
module Api.Convert
  ( -- * Amount conversions
    amountToJSON
  , mixedAmountToJSON
    -- * Hledger helpers
  , ledgerAccountBalance
  , toStatusJSON
  ) where

import Data.List (find)
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
