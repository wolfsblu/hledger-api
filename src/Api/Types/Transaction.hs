{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Types.Transaction
  ( TransactionJSON(..)
  , PostingJSON(..)
  , TransactionDetail
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

-- | Transaction detail (same as TransactionJSON for now)
type TransactionDetail = TransactionJSON
