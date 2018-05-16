{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.GS1.EventId where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Swagger
import           Data.UUID       as UUID
import           GHC.Generics
import           Web.HttpApiData


newtype EventId = EventId {unEventId :: UUID}
  deriving (Show, Eq, Generic, Read)


makeWrapped ''EventId

instance FromHttpApiData EventId where
  parseQueryParam httpData = EventId <$> parseQueryParam httpData

$(deriveJSON defaultOptions ''EventId)
instance ToSchema EventId

instance ToParamSchema EventId where
  toParamSchema _ = mempty
    & type_ .~ SwaggerString
