{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.GS1.DWhen where

import           Control.Lens
-- import           Control.Monad.Error.Lens
-- import           Control.Monad.Except     (MonadError)
import           Data.Either.Combinators
import           Data.Time
import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Swagger
-- import qualified Data.Text as T

-- import Database.SQLite.Simple.ToField
-- import Data.Aeson.Text
-- import Data.ByteString.Char8 (pack)
-- import qualified Data.Text.Lazy as TxtL

import Data.GS1.EPC

data DWhen = DWhen
  {
    _eventTime  :: EPCISTime
  , _recordTime :: Maybe EPCISTime-- minOccurs = 0
  , _timeZone   :: TimeZone
  }
  deriving (Show, Eq, Generic)


$(deriveJSON defaultOptions ''DWhen)
instance ToSchema DWhen
makeClassy ''DWhen

-- DELETEME
-- |eventTime, recordTime, the timezone is eventTime's
-- XXX a lot of this switching between Either and Maybe can be rewritten using _Left and _Right prisms
mkDWhen :: String -> String -> Maybe DWhen
mkDWhen a b = let et = parseStr2Time a :: Either EPCISTimeError EPCISTime
                  rt = parseStr2Time b :: Either EPCISTimeError EPCISTime
                  tz = parseStr2TimeZone a :: Either EPCISTimeError TimeZone in
                  case et of
                    Right et' ->
                        case rt of
                        Right rt' ->
                          let diff = diffUTCTime et' rt' in
                                      if diff < 0
                                      then Just $ DWhen et' (Just rt') (fromRight' tz) else Nothing
                        _         -> Just $ DWhen et' Nothing (fromRight' tz)
                    _         -> Nothing

-- DELETEME
-- |use it when event time and record time are the same
mkDWhen' :: String -> Maybe DWhen
mkDWhen' s = let t = parseStr2Time s :: Either EPCISTimeError EPCISTime
                 tz = parseStr2TimeZone s :: Either EPCISTimeError TimeZone in
                 case t of
                   Right t' -> Just $ DWhen t' (Just t') (fromRight' tz)
                   _        -> Nothing
