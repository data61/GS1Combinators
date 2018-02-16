{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-} --, UndecidableInstances #-}

-- | module for an EventID
-- just contains the EventID type

module Data.GS1.EventID where

import           Control.Lens
import           Control.Monad (liftM)
import           Data.UUID as UUID
import           Data.Aeson
import           Data.Aeson.TH
import           GHC.Generics
import           Data.Swagger
import           Database.SQLite.Simple.ToField
import           Data.ByteString.Char8 (pack)
import           Web.HttpApiData

newtype EventID = EventID {getEventId :: UUID}
  deriving (Show, Eq, Generic)

makeWrapped ''EventID

instance FromHttpApiData EventID where
  parseQueryParam httpData = liftM EventID $ parseQueryParam httpData

instance ToField EventID where
  toField = toField . pack . show


--instance ToSchema UUID
$(deriveJSON defaultOptions ''EventID)
instance ToSchema EventID

instance ToParamSchema EventID where
  toParamSchema _ = mempty
    & type_ .~ SwaggerString
