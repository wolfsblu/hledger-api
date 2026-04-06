{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Swagger
  ( apiSwagger
  ) where

import Control.Lens
import Data.OpenApi
import Data.Proxy (Proxy(..))
import Servant (type (:>))
import Servant.OpenApi

import qualified Data.HashMap.Strict.InsOrd as IOHM
import Servant.Multipart (MultipartForm, Mem, MultipartData)

import Api (CoreAPI)

-- | Orphan HasOpenApi instance for MultipartForm.
-- servant-openapi3 does not provide this; we annotate the endpoint manually.
instance HasOpenApi api => HasOpenApi (MultipartForm Mem (MultipartData Mem) :> api) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy api)
    & allOperations . requestBody ?~ Inline (mempty
        & required ?~ True
        & content .~ IOHM.fromList
            [ ( "multipart/form-data"
              , mempty & schema ?~ Inline (mempty
                  & type_ ?~ OpenApiObject
                  & properties .~ IOHM.fromList
                      [ ("file", Inline $ mempty
                          & type_ ?~ OpenApiString
                          & format ?~ "binary")
                      ])
              )
            ])

-- | Generate OpenAPI spec for the API
apiSwagger :: OpenApi
apiSwagger = toOpenApi (Proxy :: Proxy CoreAPI)
  & info . title .~ "hledger API"
  & info . version .~ "0.1.0"
  & info . description ?~ "REST API for hledger journal access"
  & info . license ?~ ("AGPL-3.0" & url ?~ URL "https://opensource.org/license/agpl-v3")
