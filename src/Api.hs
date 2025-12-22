{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( -- * API Types
    API
  , CoreAPI
  , RootAPI(..)
  , AccountsAPI(..)
  , TransactionsAPI(..)
  , ReportsAPI(..)
  , MetaAPI(..)
    -- * Re-exports
  , module Api.Types
  ) where

import Data.OpenApi (OpenApi)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import Servant

import Api.Types

-- | Core API type (without swagger endpoint)
type CoreAPI = "api" :> "v1" :> NamedRoutes RootAPI

-- | Full API type including OpenAPI spec endpoint
type API = CoreAPI
      :<|> "openapi.json" :> Get '[JSON] OpenApi

-- | Root API with named routes
data RootAPI mode = RootAPI
  { accounts     :: mode :- "accounts" :> NamedRoutes AccountsAPI
  , transactions :: mode :- "transactions" :> NamedRoutes TransactionsAPI
  , reports      :: mode :- "reports" :> NamedRoutes ReportsAPI
  , meta         :: mode :- NamedRoutes MetaAPI
  } deriving Generic

-- | Accounts API
data AccountsAPI mode = AccountsAPI
  { listAccounts :: mode :-
      QueryParam "depth" Int :>
      QueryParam "type" Text :>
      Get '[JSON] [AccountInfo]

  , getAccount :: mode :-
      Capture "name" Text :>
      Get '[JSON] AccountDetail

  , getAccountBalance :: mode :-
      Capture "name" Text :>
      "balance" :>
      QueryParam "from" Day :>
      QueryParam "to" Day :>
      Get '[JSON] BalanceReport

  , getAccountRegister :: mode :-
      Capture "name" Text :>
      "register" :>
      QueryParam "from" Day :>
      QueryParam "to" Day :>
      QueryParam "limit" Int :>
      QueryParam "offset" Int :>
      Get '[JSON] RegisterReport
  } deriving Generic

-- | Transactions API
data TransactionsAPI mode = TransactionsAPI
  { listTransactions :: mode :-
      QueryParam "from" Day :>
      QueryParam "to" Day :>
      QueryParam "account" Text :>
      QueryParam "description" Text :>
      QueryParam "limit" Int :>
      QueryParam "offset" Int :>
      Get '[JSON] (PaginatedResponse TransactionJSON)

  , getTransaction :: mode :-
      Capture "index" Int :>
      Get '[JSON] TransactionDetail
  } deriving Generic

-- | Reports API
data ReportsAPI mode = ReportsAPI
  { getBalanceSheet :: mode :-
      "balance-sheet" :>
      QueryParam "date" Day :>
      QueryParam "depth" Int :>
      Get '[JSON] BalanceSheetReport

  , getIncomeStatement :: mode :-
      "income-statement" :>
      QueryParam "from" Day :>
      QueryParam "to" Day :>
      QueryParam "depth" Int :>
      Get '[JSON] IncomeStatementReport

  , getCashFlow :: mode :-
      "cash-flow" :>
      QueryParam "from" Day :>
      QueryParam "to" Day :>
      Get '[JSON] CashFlowReport
  } deriving Generic

-- | Metadata API
data MetaAPI mode = MetaAPI
  { getVersion :: mode :-
      "version" :>
      Get '[JSON] VersionInfo

  , getCommodities :: mode :-
      "commodities" :>
      Get '[JSON] [CommodityInfo]

  , getPayees :: mode :-
      "payees" :>
      Get '[JSON] [Text]

  , getTags :: mode :-
      "tags" :>
      Get '[JSON] [Text]
  } deriving Generic
