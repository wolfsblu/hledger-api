{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Types
  ( -- * Account Types
    AccountInfo(..)
  , AccountDetail(..)
  , BalanceReport(..)
  , BalanceItem(..)
  , RegisterReport(..)
  , RegisterEntry(..)
    -- * Transaction Types
  , TransactionJSON(..)
  , PostingJSON(..)
  , TransactionDetail(..)
    -- * Amount Types
  , AmountJSON(..)
  , MixedAmountJSON(..)
    -- * Report Types
  , BalanceSheetReport(..)
  , IncomeStatementReport(..)
  , CashFlowReport(..)
  , ReportSection(..)
  , ReportRow(..)
    -- * Meta Types
  , VersionInfo(..)
  , CommodityInfo(..)
    -- * Pagination
  , PaginatedResponse(..)
    -- * Status
  , StatusJSON(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), object, withText)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson

-- | Transaction status for JSON
data StatusJSON = Unmarked | Pending | Cleared
  deriving (Show, Eq, Generic)

instance ToJSON StatusJSON where
  toJSON Unmarked = Aeson.String "unmarked"
  toJSON Pending  = Aeson.String "pending"
  toJSON Cleared  = Aeson.String "cleared"

instance FromJSON StatusJSON where
  parseJSON = withText "StatusJSON" $ \t ->
    case t of
      "unmarked" -> pure Unmarked
      "pending"  -> pure Pending
      "cleared"  -> pure Cleared
      _          -> fail "Invalid status"

-- | Monetary amount in JSON format
data AmountJSON = AmountJSON
  { amountQuantity  :: Scientific
  , amountCommodity :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON AmountJSON where
  toJSON a = object
    [ "quantity"  .= amountQuantity a
    , "commodity" .= amountCommodity a
    ]

instance FromJSON AmountJSON where
  parseJSON = Aeson.withObject "AmountJSON" $ \v ->
    AmountJSON <$> v .: "quantity" <*> v .: "commodity"

-- | Mixed amount (multiple commodities)
newtype MixedAmountJSON = MixedAmountJSON { unMixedAmount :: [AmountJSON] }
  deriving (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Account summary info
data AccountInfo = AccountInfo
  { accountName       :: Text
  , accountFullName   :: Text
  , accountType       :: Maybe Text
  , accountBalance    :: MixedAmountJSON
  , accountSubCount   :: Int
  , accountDepth      :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON AccountInfo where
  toJSON a = object
    [ "name"        .= accountName a
    , "fullName"    .= accountFullName a
    , "type"        .= accountType a
    , "balance"     .= accountBalance a
    , "subAccounts" .= accountSubCount a
    , "depth"       .= accountDepth a
    ]

-- | Detailed account info
data AccountDetail = AccountDetail
  { detailName        :: Text
  , detailFullName    :: Text
  , detailType        :: Maybe Text
  , detailBalance     :: MixedAmountJSON
  , detailSubAccounts :: [Text]
  , detailParent      :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON AccountDetail where
  toJSON a = object
    [ "name"        .= detailName a
    , "fullName"    .= detailFullName a
    , "type"        .= detailType a
    , "balance"     .= detailBalance a
    , "subAccounts" .= detailSubAccounts a
    , "parent"      .= detailParent a
    ]

-- | A posting within a transaction
data PostingJSON = PostingJSON
  { postingAccount :: Text
  , postingAmount  :: MixedAmountJSON
  , postingStatus  :: StatusJSON
  } deriving (Show, Eq, Generic)

instance ToJSON PostingJSON where
  toJSON p = object
    [ "account" .= postingAccount p
    , "amount"  .= postingAmount p
    , "status"  .= postingStatus p
    ]

-- | Transaction in JSON format
data TransactionJSON = TransactionJSON
  { txnIndex       :: Int
  , txnDate        :: Day
  , txnDate2       :: Maybe Day
  , txnStatus      :: StatusJSON
  , txnCode        :: Text
  , txnDescription :: Text
  , txnComment     :: Text
  , txnTags        :: [(Text, Text)]
  , txnPostings    :: [PostingJSON]
  } deriving (Show, Eq, Generic)

instance ToJSON TransactionJSON where
  toJSON t = object
    [ "index"       .= txnIndex t
    , "date"        .= txnDate t
    , "date2"       .= txnDate2 t
    , "status"      .= txnStatus t
    , "code"        .= txnCode t
    , "description" .= txnDescription t
    , "comment"     .= txnComment t
    , "tags"        .= map (\(k,v) -> object ["name" .= k, "value" .= v]) (txnTags t)
    , "postings"    .= txnPostings t
    ]

-- | Transaction detail (same as TransactionJSON for now)
type TransactionDetail = TransactionJSON

-- | Balance report item
data BalanceItem = BalanceItem
  { balanceDate   :: Day
  , balanceAmount :: MixedAmountJSON
  } deriving (Show, Eq, Generic)

instance ToJSON BalanceItem where
  toJSON b = object
    [ "date"    .= balanceDate b
    , "balance" .= balanceAmount b
    ]

-- | Historical balance report
data BalanceReport = BalanceReport
  { balanceAccount   :: Text
  , balanceHistory   :: [BalanceItem]
  , balanceTotal     :: MixedAmountJSON
  } deriving (Show, Eq, Generic)

instance ToJSON BalanceReport where
  toJSON b = object
    [ "account" .= balanceAccount b
    , "history" .= balanceHistory b
    , "total"   .= balanceTotal b
    ]

-- | Register entry (transaction affecting an account)
data RegisterEntry = RegisterEntry
  { regDate        :: Day
  , regDescription :: Text
  , regOtherAccounts :: [Text]
  , regAmount      :: MixedAmountJSON
  , regBalance     :: MixedAmountJSON
  } deriving (Show, Eq, Generic)

instance ToJSON RegisterEntry where
  toJSON r = object
    [ "date"          .= regDate r
    , "description"   .= regDescription r
    , "otherAccounts" .= regOtherAccounts r
    , "amount"        .= regAmount r
    , "balance"       .= regBalance r
    ]

-- | Register report (list of entries with running balance)
data RegisterReport = RegisterReport
  { registerAccount :: Text
  , registerEntries :: [RegisterEntry]
  , registerTotal   :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON RegisterReport where
  toJSON r = object
    [ "account" .= registerAccount r
    , "entries" .= registerEntries r
    , "total"   .= registerTotal r
    ]

-- | Row in a financial report
data ReportRow = ReportRow
  { rowAccount :: Text
  , rowAmount  :: MixedAmountJSON
  , rowDepth   :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON ReportRow where
  toJSON r = object
    [ "account" .= rowAccount r
    , "amount"  .= rowAmount r
    , "depth"   .= rowDepth r
    ]

-- | Section in a financial report
data ReportSection = ReportSection
  { sectionTitle :: Text
  , sectionRows  :: [ReportRow]
  , sectionTotal :: MixedAmountJSON
  } deriving (Show, Eq, Generic)

instance ToJSON ReportSection where
  toJSON s = object
    [ "title" .= sectionTitle s
    , "rows"  .= sectionRows s
    , "total" .= sectionTotal s
    ]

-- | Balance sheet report
data BalanceSheetReport = BalanceSheetReport
  { bsDate        :: Day
  , bsAssets      :: ReportSection
  , bsLiabilities :: ReportSection
  , bsEquity      :: ReportSection
  , bsNetWorth    :: MixedAmountJSON
  } deriving (Show, Eq, Generic)

instance ToJSON BalanceSheetReport where
  toJSON b = object
    [ "date"        .= bsDate b
    , "assets"      .= bsAssets b
    , "liabilities" .= bsLiabilities b
    , "equity"      .= bsEquity b
    , "netWorth"    .= bsNetWorth b
    ]

-- | Income statement report
data IncomeStatementReport = IncomeStatementReport
  { isFromDate  :: Day
  , isToDate    :: Day
  , isRevenues  :: ReportSection
  , isExpenses  :: ReportSection
  , isNetIncome :: MixedAmountJSON
  } deriving (Show, Eq, Generic)

instance ToJSON IncomeStatementReport where
  toJSON i = object
    [ "from"      .= isFromDate i
    , "to"        .= isToDate i
    , "revenues"  .= isRevenues i
    , "expenses"  .= isExpenses i
    , "netIncome" .= isNetIncome i
    ]

-- | Cash flow report
data CashFlowReport = CashFlowReport
  { cfFromDate   :: Day
  , cfToDate     :: Day
  , cfOperating  :: ReportSection
  , cfInvesting  :: ReportSection
  , cfFinancing  :: ReportSection
  , cfNetChange  :: MixedAmountJSON
  } deriving (Show, Eq, Generic)

instance ToJSON CashFlowReport where
  toJSON c = object
    [ "from"      .= cfFromDate c
    , "to"        .= cfToDate c
    , "operating" .= cfOperating c
    , "investing" .= cfInvesting c
    , "financing" .= cfFinancing c
    , "netChange" .= cfNetChange c
    ]

-- | API version info
data VersionInfo = VersionInfo
  { versionApi     :: Text
  , versionHledger :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON VersionInfo where
  toJSON v = object
    [ "api"     .= versionApi v
    , "hledger" .= versionHledger v
    ]

-- | Commodity info
data CommodityInfo = CommodityInfo
  { commoditySymbol    :: Text
  , commodityPrecision :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON CommodityInfo where
  toJSON c = object
    [ "symbol"    .= commoditySymbol c
    , "precision" .= commodityPrecision c
    ]

-- | Paginated response wrapper
data PaginatedResponse a = PaginatedResponse
  { pageData   :: [a]
  , pageTotal  :: Int
  , pageLimit  :: Int
  , pageOffset :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON a => ToJSON (PaginatedResponse a) where
  toJSON p = object
    [ "data"   .= pageData p
    , "total"  .= pageTotal p
    , "limit"  .= pageLimit p
    , "offset" .= pageOffset p
    ]
