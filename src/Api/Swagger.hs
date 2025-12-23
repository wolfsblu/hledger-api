{-# LANGUAGE OverloadedStrings #-}

module Api.Swagger
  ( apiSwagger
  ) where

import Control.Lens
import Data.OpenApi
import Data.Proxy (Proxy(..))
import Servant.OpenApi

import Api (CoreAPI)

-- | Generate OpenAPI spec for the API
apiSwagger :: OpenApi
apiSwagger = toOpenApi (Proxy :: Proxy CoreAPI)
  & info . title .~ "hledger API"
  & info . version .~ "0.1.0"
  & info . description ?~ "REST API for hledger journal access"
  & info . license ?~ ("AGPL-3.0" & url ?~ URL "https://opensource.org/license/agpl-v3")
