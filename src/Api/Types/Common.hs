{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Types.Common
  ( -- * Amount Types
    AmountJSON(..)
  , MixedAmountJSON(..)
    -- * Status
  , StatusJSON(..)
    -- * Pagination
  , PaginatedResponse(..)
  ) where

import Control.Lens hiding ((.=))
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), object, withText)
import Data.OpenApi
import Data.Proxy (Proxy(..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict.InsOrd as IOHM

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

instance ToSchema StatusJSON where
  declareNamedSchema _ = return $ NamedSchema (Just "Status") $ mempty
    & type_ ?~ OpenApiString
    & enum_ ?~ ["unmarked", "pending", "cleared"]
    & description ?~ "Transaction or posting status"

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

-- | Mixed amount (multiple commodities)
newtype MixedAmountJSON = MixedAmountJSON { unMixedAmount :: [AmountJSON] }
  deriving (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

instance ToSchema MixedAmountJSON where
  declareNamedSchema _ = do
    amountRef <- declareSchemaRef (Proxy :: Proxy AmountJSON)
    return $ NamedSchema (Just "MixedAmount") $ mempty
      & type_ ?~ OpenApiArray
      & items ?~ OpenApiItemsObject amountRef
      & description ?~ "A multi-currency amount (list of amounts)"

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
