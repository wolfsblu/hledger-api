{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Types.Account
  ( AccountInfo(..)
  , AccountDetail(..)
  , BalanceReport(..)
  , BalanceItem(..)
  , RegisterReport(..)
  , RegisterEntry(..)
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

instance ToSchema AccountInfo where
  declareNamedSchema _ = do
    mixedRef <- declareSchemaRef (Proxy :: Proxy MixedAmountJSON)
    return $ NamedSchema (Just "AccountInfo") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~ IOHM.fromList
          [ ("name", Inline $ mempty & type_ ?~ OpenApiString)
          , ("fullName", Inline $ mempty & type_ ?~ OpenApiString)
          , ("type", Inline $ mempty & type_ ?~ OpenApiString)
          , ("balance", mixedRef)
          , ("subAccounts", Inline $ mempty & type_ ?~ OpenApiInteger)
          , ("depth", Inline $ mempty & type_ ?~ OpenApiInteger)
          ]
      & required .~ ["name", "fullName", "balance", "subAccounts", "depth"]

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

instance ToSchema AccountDetail where
  declareNamedSchema _ = do
    mixedRef <- declareSchemaRef (Proxy :: Proxy MixedAmountJSON)
    return $ NamedSchema (Just "AccountDetail") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~ IOHM.fromList
          [ ("name", Inline $ mempty & type_ ?~ OpenApiString)
          , ("fullName", Inline $ mempty & type_ ?~ OpenApiString)
          , ("type", Inline $ mempty & type_ ?~ OpenApiString)
          , ("balance", mixedRef)
          , ("subAccounts", Inline $ mempty & type_ ?~ OpenApiArray
                                           & items ?~ OpenApiItemsObject (Inline $ mempty & type_ ?~ OpenApiString))
          , ("parent", Inline $ mempty & type_ ?~ OpenApiString)
          ]

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

instance ToSchema BalanceItem where
  declareNamedSchema _ = do
    mixedRef <- declareSchemaRef (Proxy :: Proxy MixedAmountJSON)
    return $ NamedSchema (Just "BalanceItem") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~ IOHM.fromList
          [ ("date", Inline $ mempty & type_ ?~ OpenApiString & format ?~ "date")
          , ("balance", mixedRef)
          ]
      & required .~ ["date", "balance"]

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

instance ToSchema BalanceReport where
  declareNamedSchema _ = do
    mixedRef <- declareSchemaRef (Proxy :: Proxy MixedAmountJSON)
    itemRef <- declareSchemaRef (Proxy :: Proxy BalanceItem)
    return $ NamedSchema (Just "BalanceReport") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~ IOHM.fromList
          [ ("account", Inline $ mempty & type_ ?~ OpenApiString)
          , ("history", Inline $ mempty & type_ ?~ OpenApiArray
                                        & items ?~ OpenApiItemsObject itemRef)
          , ("total", mixedRef)
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

instance ToSchema RegisterEntry where
  declareNamedSchema _ = do
    mixedRef <- declareSchemaRef (Proxy :: Proxy MixedAmountJSON)
    return $ NamedSchema (Just "RegisterEntry") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~ IOHM.fromList
          [ ("date", Inline $ mempty & type_ ?~ OpenApiString & format ?~ "date")
          , ("description", Inline $ mempty & type_ ?~ OpenApiString)
          , ("otherAccounts", Inline $ mempty & type_ ?~ OpenApiArray)
          , ("amount", mixedRef)
          , ("balance", mixedRef)
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

instance ToSchema RegisterReport where
  declareNamedSchema _ = do
    entryRef <- declareSchemaRef (Proxy :: Proxy RegisterEntry)
    return $ NamedSchema (Just "RegisterReport") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~ IOHM.fromList
          [ ("account", Inline $ mempty & type_ ?~ OpenApiString)
          , ("entries", Inline $ mempty & type_ ?~ OpenApiArray
                                        & items ?~ OpenApiItemsObject entryRef)
          , ("total", Inline $ mempty & type_ ?~ OpenApiInteger)
          ]
