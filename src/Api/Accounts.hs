{-# LANGUAGE OverloadedStrings #-}

module Api.Accounts
  ( accountsHandlers
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.List (find, sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, UTCTime(..), getCurrentTime)
import Servant
import Servant.Server.Generic (AsServerT)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Hledger as H

import Api (AccountsAPI(..))
import Api.Types
import App (AppM, getJournal)

-- | Handlers for the accounts API
accountsHandlers :: AccountsAPI (AsServerT AppM)
accountsHandlers = AccountsAPI
  { listAccounts       = handleListAccounts
  , getAccount         = handleGetAccount
  , getAccountBalance  = handleGetAccountBalance
  , getAccountRegister = handleGetAccountRegister
  }

-- | List all accounts
handleListAccounts :: Maybe Int -> Maybe Text -> AppM [AccountInfo]
handleListAccounts mDepth mType = do
  journal <- getJournal
  let accts = H.journalAccountNames journal
      maxDepth = fromMaybe 9999 mDepth
      ledger = H.ledgerFromJournal H.Any journal
      filtered = filter (matchesDepth maxDepth) accts
  return $ map (toAccountInfo ledger) filtered
  where
    matchesDepth maxD name = H.accountNameLevel name <= maxD

-- | Get account details
handleGetAccount :: Text -> AppM AccountDetail
handleGetAccount name = do
  journal <- getJournal
  let accts = H.journalAccountNames journal
  if name `elem` accts
    then do
      let ledger = H.ledgerFromJournal H.Any journal
          balance = ledgerAccountBalance ledger name
          subs = filter (isDirectChild name) accts
          parent = H.parentAccountName name
      return AccountDetail
        { detailName      = H.accountLeafName name
        , detailFullName  = name
        , detailType      = Nothing  -- Would need account declarations
        , detailBalance   = mixedAmountToJSON balance
        , detailSubAccounts = subs
        , detailParent    = if T.null parent then Nothing else Just parent
        }
    else throwError err404 { errBody = "Account not found" }
  where
    isDirectChild parent child =
      H.parentAccountName child == parent

-- | Get account balance over time
handleGetAccountBalance :: Text -> Maybe Day -> Maybe Day -> AppM BalanceReport
handleGetAccountBalance name mFrom mTo = do
  journal <- getJournal
  today <- liftIO $ utctDay <$> getCurrentTime
  let journalStart = fromMaybe today (H.journalStartDate False journal)
      fromDate = fromMaybe journalStart mFrom
      toDate = fromMaybe today mTo
      ledger = H.ledgerFromJournal H.Any journal
      totalBalance = ledgerAccountBalance ledger name
  -- For now, return just the current total balance
  -- A full implementation would compute balance at each date
  return BalanceReport
    { balanceAccount = name
    , balanceHistory = [BalanceItem toDate (mixedAmountToJSON totalBalance)]
    , balanceTotal   = mixedAmountToJSON totalBalance
    }

-- | Get account register (transactions affecting this account)
handleGetAccountRegister
  :: Text -> Maybe Day -> Maybe Day -> Maybe Int -> Maybe Int
  -> AppM RegisterReport
handleGetAccountRegister name mFrom mTo mLimit mOffset = do
  journal <- getJournal
  today <- liftIO $ utctDay <$> getCurrentTime
  let journalStart = fromMaybe today (H.journalStartDate False journal)
      fromDate = fromMaybe journalStart mFrom
      toDate = fromMaybe today mTo
      limit = fromMaybe 100 mLimit
      offset = fromMaybe 0 mOffset
      -- Get all transactions and filter for ones affecting this account
      txns = H.jtxns journal
      relevant = filter (affectsAccount name) txns
      inRange = filter (inDateRange fromDate toDate) relevant
      sorted = sortOn H.tdate inRange
      total = length sorted
      paged = take limit $ drop offset sorted
      entries = zipWith (toRegisterEntry name) (runningBalances name sorted) paged
  return RegisterReport
    { registerAccount = name
    , registerEntries = drop offset $ take (offset + limit) entries
    , registerTotal   = total
    }
  where
    affectsAccount acct txn =
      any (\p -> H.paccount p == acct || T.isPrefixOf (acct <> T.pack ":") (H.paccount p))
          (H.tpostings txn)

    inDateRange from to txn =
      let d = H.tdate txn
      in d >= from && d <= to

-- | Convert hledger account to API AccountInfo
toAccountInfo :: H.Ledger -> H.AccountName -> AccountInfo
toAccountInfo ledger name =
  AccountInfo
    { accountName     = H.accountLeafName name
    , accountFullName = name
    , accountType     = Nothing  -- Would need account type from declarations
    , accountBalance  = mixedAmountToJSON $ ledgerAccountBalance ledger name
    , accountSubCount = length $ filter (isDirectChild name) (H.ledgerAccountNames ledger)
    , accountDepth    = H.accountNameLevel name
    }
  where
    isDirectChild parent child =
      H.parentAccountName child == parent

-- | Convert a transaction to a register entry
toRegisterEntry :: Text -> H.MixedAmount -> H.Transaction -> RegisterEntry
toRegisterEntry acct runningBal txn =
  let postings = filter (\p -> H.paccount p == acct ||
                               T.isPrefixOf (acct <> T.pack ":") (H.paccount p))
                        (H.tpostings txn)
      amount = mconcat $ map H.pamount postings
      others = map H.paccount $
               filter (\p -> not $ H.paccount p == acct ||
                             T.isPrefixOf (acct <> T.pack ":") (H.paccount p))
                      (H.tpostings txn)
  in RegisterEntry
    { regDate         = H.tdate txn
    , regDescription  = H.tdescription txn
    , regOtherAccounts = others
    , regAmount       = mixedAmountToJSON amount
    , regBalance      = mixedAmountToJSON runningBal
    }

-- | Calculate running balances for an account
runningBalances :: Text -> [H.Transaction] -> [H.MixedAmount]
runningBalances acct txns =
  scanl1 (<>) $ map (getAccountAmount acct) txns
  where
    getAccountAmount acc txn =
      mconcat $ map H.pamount $
      filter (\p -> H.paccount p == acc || T.isPrefixOf (acc <> T.pack ":") (H.paccount p))
             (H.tpostings txn)

-- | Get the balance of an account in a ledger
ledgerAccountBalance :: H.Ledger -> H.AccountName -> H.MixedAmount
ledgerAccountBalance ledger name =
  case find (\acct -> H.aname acct == name) (H.laccounts ledger) of
    Just acct -> H.aibalance acct
    Nothing   -> mempty

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
