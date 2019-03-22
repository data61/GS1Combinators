{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Data.GS1.DWhen (DWhen(..)) where

import           Data.Aeson
import           Data.GS1.EPC
import           Data.Swagger
import           Data.Time
import           GHC.Generics

import qualified Data.Text    as T

import           Control.Lens hiding ((.=))

data DWhen = DWhen
  { _eventTime  :: EPCISTime
  , _recordTime :: Maybe EPCISTime-- minOccurs = 0
  , _timeZone   :: TimeZone
  }
  deriving (Show, Eq, Generic)

instance FromJSON DWhen where
  parseJSON = withObject "BizLocation" $ \v -> DWhen
    <$> v .: "eventTime"
    <*> v .:? "recordTime"
    <*> v .: "eventTimeZoneOffset"

instance ToJSON DWhen where
  toJSON (DWhen eTime rTime etOffset) = object
    [ "eventTime" .= eTime
    , "recordTime" .= rTime
    , "eventTimeZoneOffset" .= etOffset
    ]

-- copied from
-- https://hackage.haskell.org/package/swagger2-2.1.3/docs/src/Data.Swagger.Internal.Schema.html#line-477
named :: T.Text -> Schema -> NamedSchema
named n = NamedSchema (Just n)

timeSchema :: T.Text -> Schema
timeSchema fmt = mempty
  & type_ .~ SwaggerString
  & format ?~ fmt


instance FromJSON TimeZone where
  parseJSON = withText "TimeZone" $ \str -> case ((parseTimeM True defaultTimeLocale "%z" (T.unpack str)) :: Maybe TimeZone) of
    Just t  -> pure t
    Nothing -> fail $ "Failed to parse timezone from: " <> T.unpack str

instance ToJSON TimeZone where
  toJSON = String . offset
    where
      -- timeZoneOffsetString from Data.Time.LocalTime does not support the colon between hours & minutes
      offset :: TimeZone -> T.Text
      offset (TimeZone t _ _) =
        let
          p = if t < 0 then '-' else '+'
          valueS = show ((div t 60) * 100 + (mod t 60))
          [a,b,c,d] = replicate (4 - length valueS) '0' ++ valueS
        in
          T.pack [ p, a, b, ':', c, d]
        

instance ToSchema TimeZone where
  declareNamedSchema _ = pure $ named "TimeZone" $ timeSchema "date-time"

instance ToSchema DWhen
