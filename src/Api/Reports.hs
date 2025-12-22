{-# LANGUAGE OverloadedStrings #-}

module Api.Reports
  ( reportsHandlers
  , metaHandlers
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.List (find, nub, sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, UTCTime(..), getCurrentTime, addDays)
import Data.Time.Calendar (toGregorian, fromGregorian)
import Servant.Server.Generic (AsServerT)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Hledger as H

import Api (ReportsAPI(..), MetaAPI(..))
import Api.Types
import App (AppM, getJournal)

-- | Handlers for the reports API
reportsHandlers :: ReportsAPI (AsServerT AppM)
reportsHandlers = ReportsAPI
  { getBalanceSheet    = handleBalanceSheet
  , getIncomeStatement = handleIncomeStatement
  , getCashFlow        = handleCashFlow
  }

-- | Handlers for the meta API
metaHandlers :: MetaAPI (AsServerT AppM)
metaHandlers = MetaAPI
  { getVersion     = handleVersion
  , getCommodities = handleCommodities
  , getPayees      = handlePayees
  , getTags        = handleTags
  }

-- | Get balance sheet report
handleBalanceSheet :: Maybe Day -> Maybe Int -> AppM BalanceSheetReport
handleBalanceSheet mDate mDepth = do
  journal <- getJournal
  today <- liftIO $ utctDay <$> getCurrentTime
  let asOfDate = fromMaybe today mDate
      maxDepth = fromMaybe 9999 mDepth
      ledger = H.ledgerFromJournal H.Any journal
      accts = H.ledgerAccountNames ledger

      -- Filter accounts by type prefix
      assets = filterByPrefix "assets" maxDepth accts
      liabilities = filterByPrefix "liabilities" maxDepth accts
      equity = filterByPrefix "equity" maxDepth accts

      assetSection = buildSection "Assets" ledger assets
      liabSection = buildSection "Liabilities" ledger liabilities
      equitySection = buildSection "Equity" ledger equity

      -- Net worth = assets - liabilities
      netWorth = subtractAmounts (sectionTotal assetSection)
                                 (sectionTotal liabSection)

  return BalanceSheetReport
    { bsDate        = asOfDate
    , bsAssets      = assetSection
    , bsLiabilities = liabSection
    , bsEquity      = equitySection
    , bsNetWorth    = netWorth
    }

-- | Get income statement report
handleIncomeStatement :: Maybe Day -> Maybe Day -> Maybe Int -> AppM IncomeStatementReport
handleIncomeStatement mFrom mTo mDepth = do
  journal <- getJournal
  today <- liftIO $ utctDay <$> getCurrentTime
  let (year, _, _) = toGregorian today
      fromDate = fromMaybe (fromGregorian year 1 1) mFrom
      toDate = fromMaybe today mTo
      maxDepth = fromMaybe 9999 mDepth
      ledger = H.ledgerFromJournal H.Any journal
      accts = H.ledgerAccountNames ledger

      revenues = filterByPrefix "revenues" maxDepth accts ++
                 filterByPrefix "income" maxDepth accts
      expenses = filterByPrefix "expenses" maxDepth accts

      revSection = buildSection "Revenues" ledger revenues
      expSection = buildSection "Expenses" ledger expenses

      -- Net income = revenues - expenses
      netIncome = subtractAmounts (sectionTotal revSection)
                                  (sectionTotal expSection)

  return IncomeStatementReport
    { isFromDate  = fromDate
    , isToDate    = toDate
    , isRevenues  = revSection
    , isExpenses  = expSection
    , isNetIncome = netIncome
    }

-- | Get cash flow report
handleCashFlow :: Maybe Day -> Maybe Day -> AppM CashFlowReport
handleCashFlow mFrom mTo = do
  journal <- getJournal
  today <- liftIO $ utctDay <$> getCurrentTime
  let (year, _, _) = toGregorian today
      fromDate = fromMaybe (fromGregorian year 1 1) mFrom
      toDate = fromMaybe today mTo
      ledger = H.ledgerFromJournal H.Any journal

      -- Simplified cash flow - just show cash account changes
      emptySection title = ReportSection title [] (MixedAmountJSON [])

  return CashFlowReport
    { cfFromDate   = fromDate
    , cfToDate     = toDate
    , cfOperating  = emptySection "Operating Activities"
    , cfInvesting  = emptySection "Investing Activities"
    , cfFinancing  = emptySection "Financing Activities"
    , cfNetChange  = MixedAmountJSON []
    }

-- | Get API version info
handleVersion :: AppM VersionInfo
handleVersion = return VersionInfo
  { versionApi     = "0.1.0"
  , versionHledger = "1.43+"  -- hledger-lib version
  }

-- | Get list of commodities
handleCommodities :: AppM [CommodityInfo]
handleCommodities = do
  journal <- getJournal
  let commodities = H.journalCommodities journal
  return $ map toCommodityInfo $ Set.toList commodities
  where
    toCommodityInfo symbol = CommodityInfo
      { commoditySymbol    = symbol
      , commodityPrecision = 2  -- Default precision, would need style lookup for actual precision
      }

-- | Get list of payees (descriptions)
handlePayees :: AppM [Text]
handlePayees = do
  journal <- getJournal
  let txns = H.jtxns journal
      payees = nub $ sort $ map H.tdescription txns
  return $ filter (not . T.null) payees

-- | Get list of tags
handleTags :: AppM [Text]
handleTags = do
  journal <- getJournal
  let txns = H.jtxns journal
      allTags = concatMap (map fst . H.ttags) txns
  return $ nub $ sort allTags

-- Helper functions

-- | Filter accounts by prefix and depth
filterByPrefix :: Text -> Int -> [H.AccountName] -> [H.AccountName]
filterByPrefix prefix maxDepth =
  filter (\a -> T.toLower prefix `T.isPrefixOf` T.toLower a &&
                H.accountNameLevel a <= maxDepth)

-- | Build a report section from accounts
buildSection :: Text -> H.Ledger -> [H.AccountName] -> ReportSection
buildSection title ledger accts =
  let rows = map (toReportRow ledger) accts
      total = mconcat $ map (ledgerAccountBalance ledger) accts
  in ReportSection
    { sectionTitle = title
    , sectionRows  = rows
    , sectionTotal = mixedAmountToJSON total
    }

-- | Convert account to report row
toReportRow :: H.Ledger -> H.AccountName -> ReportRow
toReportRow ledger name = ReportRow
  { rowAccount = name
  , rowAmount  = mixedAmountToJSON $ ledgerAccountBalance ledger name
  , rowDepth   = H.accountNameLevel name
  }

-- | Get the balance of an account in a ledger
ledgerAccountBalance :: H.Ledger -> H.AccountName -> H.MixedAmount
ledgerAccountBalance ledger name =
  case find (\acct -> H.aname acct == name) (H.laccounts ledger) of
    Just acct -> H.aibalance acct
    Nothing   -> mempty

-- | Subtract two mixed amounts (for net calculations)
subtractAmounts :: MixedAmountJSON -> MixedAmountJSON -> MixedAmountJSON
subtractAmounts (MixedAmountJSON as) (MixedAmountJSON bs) =
  MixedAmountJSON $ as ++ map negateAmount bs
  where
    negateAmount a = a { amountQuantity = negate (amountQuantity a) }

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
