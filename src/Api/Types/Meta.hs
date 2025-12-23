{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Types.Meta
  ( VersionInfo(..)
  , CommodityInfo(..)
  ) where

import Control.Lens hiding ((.=))
import Data.Aeson (ToJSON(..), (.=), object)
import Data.OpenApi
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.HashMap.Strict.InsOrd as IOHM

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

instance ToSchema VersionInfo where
  declareNamedSchema _ = return $ NamedSchema (Just "VersionInfo") $ mempty
    & type_ ?~ OpenApiObject
    & properties .~ IOHM.fromList
        [ ("api", Inline $ mempty & type_ ?~ OpenApiString)
        , ("hledger", Inline $ mempty & type_ ?~ OpenApiString)
        ]
    & required .~ ["api", "hledger"]

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

instance ToSchema CommodityInfo where
  declareNamedSchema _ = return $ NamedSchema (Just "CommodityInfo") $ mempty
    & type_ ?~ OpenApiObject
    & properties .~ IOHM.fromList
        [ ("symbol", Inline $ mempty & type_ ?~ OpenApiString)
        , ("precision", Inline $ mempty & type_ ?~ OpenApiInteger)
        ]
    & required .~ ["symbol", "precision"]
