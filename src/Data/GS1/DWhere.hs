{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Data.GS1.DWhere
  ( ReadPointLocation(..)
  , BizLocation(..)
  , DestinationLocation(..)
  , SourceLocation(..)
  , DWhere(..)
  )
  where

import           GHC.Generics

import           Data.Aeson
import           Data.GS1.EPC
import           Data.Swagger

-- | Location synonym
newtype ReadPointLocation = ReadPointLocation {unReadPointLocation :: LocationEPC}
  deriving (Show, Read, Eq, Generic, URI)
instance ToSchema ReadPointLocation

instance FromJSON ReadPointLocation where
  parseJSON v = ReadPointLocation <$> parseJSON v
instance ToJSON ReadPointLocation where
  toJSON = toJSON . unReadPointLocation

-- | Location synonym
newtype BizLocation = BizLocation {unBizLocation :: LocationEPC}
  deriving (Show, Read, Eq, Generic, URI)

instance ToSchema BizLocation

instance FromJSON BizLocation where
  parseJSON v = BizLocation <$> parseJSON v
instance ToJSON BizLocation where
  toJSON = toJSON . unBizLocation


data SourceLocation = SourceLocation
  { _sourceLocationType :: SourceDestType
  , _sourceLocationSource :: LocationEPC
  } deriving (Show, Read, Eq, Generic)

instance ToSchema SourceLocation

instance FromJSON SourceLocation where
  parseJSON = withObject "SourceLocation" $ \o ->
    SourceLocation <$> o .: "type"
                   <*> o .: "source"

instance ToJSON SourceLocation where
  toJSON (SourceLocation a b) =
    object [ "type" .= a
           , "source" .= b
           ]
    

data DestinationLocation = DestinationLocation
  { _destinationLocationType :: SourceDestType
  , _destinationLocationDestination :: LocationEPC
  } deriving (Show, Read, Eq, Generic)

instance ToSchema DestinationLocation

instance FromJSON DestinationLocation where
  parseJSON = withObject "DestinationLocation" $ \o ->
    DestinationLocation <$> o .: "type"
                        <*> o .: "destination"

instance ToJSON DestinationLocation where
  toJSON (DestinationLocation a b) =
    object [ "type" .= a
           , "destination" .= b
           ]


data DWhere = DWhere
  {
    _readPoint   :: Maybe ReadPointLocation
  , _bizLocation :: Maybe BizLocation
  , _srcType     :: [SourceLocation]
  , _destType    :: [DestinationLocation]
  }
  deriving (Show, Eq, Generic)

instance ToSchema DWhere

instance FromJSON DWhere where
  parseJSON = withObject "DWhere" $ \o ->
    DWhere <$> o .: "readPoint"
           <*> o .: "bizLocation"
           <*> o .: "sourceList"
           <*> o .: "destinationList"
  
instance ToJSON DWhere where
  toJSON (DWhere a b c d) =
    object [ "readPoint" .= a
           , "bizLocation" .= b
           , "sourceList" .= c
           , "destinationList" .= d
           ]
