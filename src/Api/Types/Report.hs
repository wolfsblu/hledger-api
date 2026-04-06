module Api.Types.Report
  ( BalanceSheetReport(..)
  , IncomeStatementReport(..)
  , CashFlowReport(..)
  , NetWorthReport(..)
  , NetWorthDataPoint(..)
  , ReportSection(..)
  , ReportRow(..)
  ) where

import Control.Lens hiding ((.=))
import Data.Aeson (ToJSON(..), (.=), object)
import Data.OpenApi
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)

import qualified Data.HashMap.Strict.InsOrd as IOHM

import Api.Types.Common

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

instance ToSchema ReportRow where
  declareNamedSchema _ = do
    mixedRef <- declareSchemaRef (Proxy :: Proxy MixedAmountJSON)
    return $ NamedSchema (Just "ReportRow") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~ IOHM.fromList
          [ ("account", Inline $ mempty & type_ ?~ OpenApiString)
          , ("amount", mixedRef)
          , ("depth", Inline $ mempty & type_ ?~ OpenApiInteger)
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

instance ToSchema ReportSection where
  declareNamedSchema _ = do
    mixedRef <- declareSchemaRef (Proxy :: Proxy MixedAmountJSON)
    rowRef <- declareSchemaRef (Proxy :: Proxy ReportRow)
    return $ NamedSchema (Just "ReportSection") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~ IOHM.fromList
          [ ("title", Inline $ mempty & type_ ?~ OpenApiString)
          , ("rows", Inline $ mempty & type_ ?~ OpenApiArray
                                     & items ?~ OpenApiItemsObject rowRef)
          , ("total", mixedRef)
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

instance ToSchema BalanceSheetReport where
  declareNamedSchema _ = do
    mixedRef <- declareSchemaRef (Proxy :: Proxy MixedAmountJSON)
    sectionRef <- declareSchemaRef (Proxy :: Proxy ReportSection)
    return $ NamedSchema (Just "BalanceSheetReport") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~ IOHM.fromList
          [ ("date", Inline $ mempty & type_ ?~ OpenApiString & format ?~ "date")
          , ("assets", sectionRef)
          , ("liabilities", sectionRef)
          , ("equity", sectionRef)
          , ("netWorth", mixedRef)
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

instance ToSchema IncomeStatementReport where
  declareNamedSchema _ = do
    mixedRef <- declareSchemaRef (Proxy :: Proxy MixedAmountJSON)
    sectionRef <- declareSchemaRef (Proxy :: Proxy ReportSection)
    return $ NamedSchema (Just "IncomeStatementReport") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~ IOHM.fromList
          [ ("from", Inline $ mempty & type_ ?~ OpenApiString & format ?~ "date")
          , ("to", Inline $ mempty & type_ ?~ OpenApiString & format ?~ "date")
          , ("revenues", sectionRef)
          , ("expenses", sectionRef)
          , ("netIncome", mixedRef)
          ]

-- | Single data point in a net worth time series
data NetWorthDataPoint = NetWorthDataPoint
  { nwpDate     :: Day
  , nwpNetWorth :: MixedAmountJSON
  } deriving (Show, Eq, Generic)

instance ToJSON NetWorthDataPoint where
  toJSON p = object
    [ "date"     .= nwpDate p
    , "netWorth" .= nwpNetWorth p
    ]

instance ToSchema NetWorthDataPoint where
  declareNamedSchema _ = do
    mixedRef <- declareSchemaRef (Proxy :: Proxy MixedAmountJSON)
    return $ NamedSchema (Just "NetWorthDataPoint") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~ IOHM.fromList
          [ ("date",     Inline $ mempty & type_ ?~ OpenApiString & format ?~ "date")
          , ("netWorth", mixedRef)
          ]

-- | Net worth time series report
data NetWorthReport = NetWorthReport
  { nwFrom       :: Day
  , nwTo         :: Day
  , nwInterval   :: Text
  , nwDataPoints :: [NetWorthDataPoint]
  } deriving (Show, Eq, Generic)

instance ToJSON NetWorthReport where
  toJSON r = object
    [ "from"       .= nwFrom r
    , "to"         .= nwTo r
    , "interval"   .= nwInterval r
    , "dataPoints" .= nwDataPoints r
    ]

instance ToSchema NetWorthReport where
  declareNamedSchema _ = do
    pointRef <- declareSchemaRef (Proxy :: Proxy NetWorthDataPoint)
    return $ NamedSchema (Just "NetWorthReport") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~ IOHM.fromList
          [ ("from",       Inline $ mempty & type_ ?~ OpenApiString & format ?~ "date")
          , ("to",         Inline $ mempty & type_ ?~ OpenApiString & format ?~ "date")
          , ("interval",   Inline $ mempty & type_ ?~ OpenApiString)
          , ("dataPoints", Inline $ mempty & type_ ?~ OpenApiArray
                                           & items ?~ OpenApiItemsObject pointRef)
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

instance ToSchema CashFlowReport where
  declareNamedSchema _ = do
    mixedRef <- declareSchemaRef (Proxy :: Proxy MixedAmountJSON)
    sectionRef <- declareSchemaRef (Proxy :: Proxy ReportSection)
    return $ NamedSchema (Just "CashFlowReport") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~ IOHM.fromList
          [ ("from", Inline $ mempty & type_ ?~ OpenApiString & format ?~ "date")
          , ("to", Inline $ mempty & type_ ?~ OpenApiString & format ?~ "date")
          , ("operating", sectionRef)
          , ("investing", sectionRef)
          , ("financing", sectionRef)
          , ("netChange", mixedRef)
          ]
