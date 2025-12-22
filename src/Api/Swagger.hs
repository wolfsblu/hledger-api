{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Swagger
  ( apiSwagger
  ) where

import Control.Lens hiding ((.=))
import Data.OpenApi
import Data.Proxy (Proxy(..))
import Servant.OpenApi

import qualified Data.HashMap.Strict.InsOrd as IOHM

import Api (CoreAPI)
import Api.Types

-- | Generate OpenAPI spec for the API
apiSwagger :: OpenApi
apiSwagger = toOpenApi (Proxy :: Proxy CoreAPI)
  & info . title .~ "hledger API"
  & info . version .~ "0.1.0"
  & info . description ?~ "REST API for hledger journal access"
  & info . license ?~ ("AGPL-3.0" & url ?~ URL "https://opensource.org/license/agpl-v3")

-- ToSchema instances for API types

instance ToSchema StatusJSON where
  declareNamedSchema _ = return $ NamedSchema (Just "Status") $ mempty
    & type_ ?~ OpenApiString
    & enum_ ?~ ["unmarked", "pending", "cleared"]
    & description ?~ "Transaction or posting status"

instance ToSchema AmountJSON where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "Amount") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~ IOHM.fromList
          [ ("quantity", Inline $ mempty & type_ ?~ OpenApiNumber)
          , ("commodity", Inline $ mempty & type_ ?~ OpenApiString)
          ]
      & required .~ ["quantity", "commodity"]
      & description ?~ "A monetary amount with commodity"

instance ToSchema MixedAmountJSON where
  declareNamedSchema _ = do
    amountRef <- declareSchemaRef (Proxy :: Proxy AmountJSON)
    return $ NamedSchema (Just "MixedAmount") $ mempty
      & type_ ?~ OpenApiArray
      & items ?~ OpenApiItemsObject amountRef
      & description ?~ "A multi-currency amount (list of amounts)"

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

instance ToSchema PostingJSON where
  declareNamedSchema _ = do
    mixedRef <- declareSchemaRef (Proxy :: Proxy MixedAmountJSON)
    statusRef <- declareSchemaRef (Proxy :: Proxy StatusJSON)
    return $ NamedSchema (Just "Posting") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~ IOHM.fromList
          [ ("account", Inline $ mempty & type_ ?~ OpenApiString)
          , ("amount", mixedRef)
          , ("status", statusRef)
          ]
      & required .~ ["account", "amount", "status"]

instance ToSchema TransactionJSON where
  declareNamedSchema _ = do
    statusRef <- declareSchemaRef (Proxy :: Proxy StatusJSON)
    postingRef <- declareSchemaRef (Proxy :: Proxy PostingJSON)
    return $ NamedSchema (Just "Transaction") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~ IOHM.fromList
          [ ("index", Inline $ mempty & type_ ?~ OpenApiInteger)
          , ("date", Inline $ mempty & type_ ?~ OpenApiString & format ?~ "date")
          , ("date2", Inline $ mempty & type_ ?~ OpenApiString & format ?~ "date")
          , ("status", statusRef)
          , ("code", Inline $ mempty & type_ ?~ OpenApiString)
          , ("description", Inline $ mempty & type_ ?~ OpenApiString)
          , ("comment", Inline $ mempty & type_ ?~ OpenApiString)
          , ("tags", Inline $ mempty & type_ ?~ OpenApiArray)
          , ("postings", Inline $ mempty & type_ ?~ OpenApiArray
                                         & items ?~ OpenApiItemsObject postingRef)
          ]
      & required .~ ["index", "date", "status", "description", "postings"]

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

instance ToSchema VersionInfo where
  declareNamedSchema _ = return $ NamedSchema (Just "VersionInfo") $ mempty
    & type_ ?~ OpenApiObject
    & properties .~ IOHM.fromList
        [ ("api", Inline $ mempty & type_ ?~ OpenApiString)
        , ("hledger", Inline $ mempty & type_ ?~ OpenApiString)
        ]
    & required .~ ["api", "hledger"]

instance ToSchema CommodityInfo where
  declareNamedSchema _ = return $ NamedSchema (Just "CommodityInfo") $ mempty
    & type_ ?~ OpenApiObject
    & properties .~ IOHM.fromList
        [ ("symbol", Inline $ mempty & type_ ?~ OpenApiString)
        , ("precision", Inline $ mempty & type_ ?~ OpenApiInteger)
        ]
    & required .~ ["symbol", "precision"]

instance ToSchema a => ToSchema (PaginatedResponse a) where
  declareNamedSchema (_ :: Proxy (PaginatedResponse a)) = do
    dataRef <- declareSchemaRef (Proxy :: Proxy [a])
    return $ NamedSchema (Just "PaginatedResponse") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~ IOHM.fromList
          [ ("data", Inline $ mempty & type_ ?~ OpenApiArray
                                     & items ?~ OpenApiItemsObject dataRef)
          , ("total", Inline $ mempty & type_ ?~ OpenApiInteger)
          , ("limit", Inline $ mempty & type_ ?~ OpenApiInteger)
          , ("offset", Inline $ mempty & type_ ?~ OpenApiInteger)
          ]
      & required .~ ["data", "total", "limit", "offset"]
