{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.GS1.EventID where

import           Control.Lens
import           Control.Monad (liftM)
import           Data.UUID as UUID
import           Data.Aeson
import           Data.Aeson.TH
import           GHC.Generics
import           Data.Swagger
import           Web.HttpApiData


newtype EventID = EventID {getEventId :: UUID}
  deriving (Show, Eq, Generic, Read)


makeWrapped ''EventID

instance FromHttpApiData EventID where
  parseQueryParam httpData = liftM EventID $ parseQueryParam httpData

$(deriveJSON defaultOptions ''EventID)
instance ToSchema EventID

instance ToParamSchema EventID where
  toParamSchema _ = mempty
    & type_ .~ SwaggerString
