module Api.Accounts
  ( accountsHandlers
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (Day, UTCTime(..), getCurrentTime)
import Servant
import Servant.Server.Generic (AsServerT)

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Hledger as H

import Api (AccountsAPI(..))
import Api.Convert
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

-- | List all accounts as a nested tree
handleListAccounts :: Maybe Int -> Maybe Text -> Maybe Text -> AppM [AccountTree]
handleListAccounts mDepth _mType mSearch = do
  journal <- getJournal
  let maxDepth = fromMaybe 9999 mDepth
      ledger = H.ledgerFromJournal H.Any journal
      allAccts = H.journalAccountNames journal
      searched = case mSearch of
        Nothing -> allAccts
        Just q  -> let lq = T.toLower q
                       matching = filter (\n -> lq `T.isInfixOf` T.toLower n) allAccts
                   in nub $ concatMap accountWithAncestors matching
      filtered = filter (\n -> H.accountNameLevel n <= maxDepth) searched
      filteredSet = Set.fromList filtered
      roots = filter (\n -> H.accountNameLevel n == 1 ||
                            not (H.parentAccountName n `Set.member` filteredSet)) filtered
  return $ map (buildAccountTree ledger filteredSet) roots

-- | Expand an account name into itself plus all ancestor accounts
accountWithAncestors :: H.AccountName -> [H.AccountName]
accountWithAncestors name =
  let parent = H.parentAccountName name
  in if T.null parent
     then [name]
     else name : accountWithAncestors parent

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
      -- DateSpan end is exclusive in hledger, so use succ toDate to include toDate
      dateQuery = H.Date $ H.DateSpan (Just (H.Exact fromDate)) (Just (H.Exact (succ toDate)))
      ledger = H.ledgerFromJournal dateQuery journal
      totalBalance = ledgerAccountBalance ledger name
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

-- | Build a nested account tree node
buildAccountTree :: H.Ledger -> Set.Set H.AccountName -> H.AccountName -> AccountTree
buildAccountTree ledger allAccts name =
  let children = sortOn id $ filter (\n -> H.parentAccountName n == name) (Set.toList allAccts)
  in AccountTree
    { treeName     = H.accountLeafName name
    , treeFullName = name
    , treeType     = Nothing
    , treeBalance  = mixedAmountToJSON $ ledgerAccountBalance ledger name
    , treeChildren = map (buildAccountTree ledger allAccts) children
    }

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

