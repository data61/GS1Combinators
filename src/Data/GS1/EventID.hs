{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.GS1.EventID where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Swagger
import           Data.UUID       as UUID
import           GHC.Generics
import           Web.HttpApiData


newtype EventID = EventID {getEventId :: UUID}
  deriving (Show, Eq, Generic, Read)


makeWrapped ''EventID

instance FromHttpApiData EventID where
  parseQueryParam httpData = EventID <$> parseQueryParam httpData

$(deriveJSON defaultOptions ''EventID)
instance ToSchema EventID

instance ToParamSchema EventID where
  toParamSchema _ = mempty
    & type_ .~ SwaggerString
