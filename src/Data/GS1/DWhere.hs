{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}

module Data.GS1.DWhere
  (ReadPointLocation(..)
  , BizLocation(..)
  , SrcDestLocation(..)
  , DWhere(..)
  )
  where

import           GHC.Generics

import           Data.Aeson
import           Data.Aeson.TH
import           Data.GS1.EPC
import           Data.Swagger

-- |Location synonym
newtype ReadPointLocation = ReadPointLocation {unReadPointLocation :: LocationEPC}
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON, URI)
instance ToSchema ReadPointLocation


-- |Location synonym
newtype BizLocation = BizLocation {unBizLocation :: LocationEPC}
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON, URI)
instance ToSchema BizLocation

newtype SrcDestLocation =
  SrcDestLocation {unSrcDestLocation :: (SourceDestType, LocationEPC)}
    deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
instance ToSchema SrcDestLocation

data DWhere = DWhere
  {
    _readPoint   :: [ReadPointLocation]
  , _bizLocation :: [BizLocation]
  , _srcType     :: [SrcDestLocation]
  , _destType    :: [SrcDestLocation]
  }
  deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions ''DWhere)
instance ToSchema DWhere
