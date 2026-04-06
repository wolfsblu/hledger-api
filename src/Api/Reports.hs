module Api.Reports
  ( reportsHandlers
  , metaHandlers
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.List (nub, partition, sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (Day, UTCTime(..), getCurrentTime)
import Data.Time.Calendar (toGregorian, fromGregorian)
import Servant.Server.Generic (AsServerT)

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Hledger as H

import Api (ReportsAPI(..), MetaAPI(..))
import Api.Convert
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
      dateQuery = H.Date $ H.DateSpan Nothing (Just (H.Exact (succ asOfDate)))
      ledger = H.ledgerFromJournal dateQuery journal
      accts = H.ledgerAccountNames ledger

      -- Filter accounts by type prefix
      assets = filterByPrefix "assets" maxDepth accts
      liabilities = filterByPrefix "liabilities" maxDepth accts
      equity = filterByPrefix "equity" maxDepth accts

      assetSection = buildSection "Assets" ledger assets
      liabSection = buildSection "Liabilities" ledger liabilities
      equitySection = buildSection "Equity" ledger equity

      -- Net worth = assets + liabilities (liabilities already carry negative sign)
      netWorth = addAmounts (sectionTotal assetSection)
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
      dateQuery = H.Date $ H.DateSpan (Just (H.Exact fromDate)) (Just (H.Exact (succ toDate)))
      ledger = H.ledgerFromJournal dateQuery journal
      accts = H.ledgerAccountNames ledger

      revenues = filterByPrefix "revenue" maxDepth accts ++
                 filterByPrefix "income" maxDepth accts
      expenses = filterByPrefix "expense" maxDepth accts

      revSection = buildSection "Revenues" ledger revenues
      expSection = buildSection "Expenses" ledger expenses

      -- Net income = -(revenues + expenses) since both carry natural signs
      -- (revenue is negative/credit, expenses positive/debit)
      netIncome = negateAmounts $ addAmounts (sectionTotal revSection)
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
  _ <- getJournal  -- TODO: Implement actual cash flow calculation
  today <- liftIO $ utctDay <$> getCurrentTime
  let (year, _, _) = toGregorian today
      fromDate = fromMaybe (fromGregorian year 1 1) mFrom
      toDate = fromMaybe today mTo
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
      -- Only sum leaf accounts to avoid double-counting, since parent
      -- account inclusive balances already contain their children
      leafAccts = filter (\a -> not $ any (\b -> a /= b && a `T.isPrefixOf` b) accts) accts
      total = mconcat $ map (ledgerAccountBalance ledger) leafAccts
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

-- | Add two mixed amounts, combining by commodity
addAmounts :: MixedAmountJSON -> MixedAmountJSON -> MixedAmountJSON
addAmounts (MixedAmountJSON as) (MixedAmountJSON bs) =
  MixedAmountJSON $ combineByCommodity (as ++ bs)

-- | Negate all amounts
negateAmounts :: MixedAmountJSON -> MixedAmountJSON
negateAmounts (MixedAmountJSON as) =
  MixedAmountJSON $ map (\a -> a { amountQuantity = negate (amountQuantity a) }) as

-- | Group amounts by commodity and sum their quantities
combineByCommodity :: [AmountJSON] -> [AmountJSON]
combineByCommodity [] = []
combineByCommodity (x:xs) =
  let (same, rest) = partition (\a -> amountCommodity a == amountCommodity x) xs
      total = foldl' (\acc a -> acc + amountQuantity a) (amountQuantity x) same
  in x { amountQuantity = total } : combineByCommodity rest
