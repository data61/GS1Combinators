{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.GS1.EventId (EventId(..)) where

import           Control.Lens
import           Data.Aeson.TH   (defaultOptions, deriveJSON)
import           Data.Swagger
import           Data.UUID       as UUID
import           GHC.Generics    (Generic)
import           Web.HttpApiData (FromHttpApiData, parseQueryParam)


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
