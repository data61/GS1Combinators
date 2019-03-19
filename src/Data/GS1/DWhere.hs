{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Data.GS1.DWhere
  ( ReadPointLocation(..)
  , BizLocation(..)
  , SrcDestLocation(..)
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

newtype SrcDestLocation =
  SrcDestLocation {unSrcDestLocation :: (SourceDestType, LocationEPC)}
    deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
instance ToSchema SrcDestLocation

data DWhere = DWhere
  {
    _readPoint   :: Maybe ReadPointLocation
  , _bizLocation :: Maybe BizLocation
  , _srcType     :: [SrcDestLocation]
  , _destType    :: [SrcDestLocation]
  }
  deriving (Show, Eq, Generic)

instance ToSchema DWhere

instance FromJSON DWhere where
  parseJSON = undefined
instance ToJSON DWhere where
  toJSON = undefined
