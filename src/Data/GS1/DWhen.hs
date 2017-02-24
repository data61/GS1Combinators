{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.GS1.DWhen where

import           Control.Lens
import           Control.Monad.Error.Lens
import           Control.Monad.Except     (MonadError)
import           Data.Either.Combinators
import           Data.Time
import           GHC.Generics

{-
   A timestamp, giving the date and time in a time zone-independent manner.
   For bindings in which fields of this type are represented textually,
   an ISO-8601 compliant representation SHOULD be used.
-}
-- |The TimeZone will be saved independently
type EPCISTime = UTCTime

data EPCISTimeError = IllegalTimeFormat deriving (Show, Eq, Generic)

makeClassyPrisms ''EPCISTimeError

-- example format: 2005-04-03T20:33:31.116-06:00
-- |parse the string to UTC time, the time zone information will be merged into the time
parseStr2Time :: (AsEPCISTimeError e, MonadError e m) => String -> m EPCISTime
parseStr2Time s = let parsed = parseTimeM True defaultTimeLocale "%FT%X%Q%z" s :: Maybe EPCISTime in
                      case parsed of
                        Just et -> pure et
                        _       -> throwing _IllegalTimeFormat ()

-- |parse the string and obtain TimeZone,
parseStr2TimeZone :: (AsEPCISTimeError e, MonadError e m) => String -> m TimeZone
parseStr2TimeZone s = let parsed = parseTimeM True defaultTimeLocale "%FT%X%Q%z" s :: Maybe ZonedTime in
                      case parsed of
                        Just t -> let tz = zonedTimeZone t :: TimeZone in
                                      pure tz
                        _      -> throwing _IllegalTimeFormat ()

data DWhen = DWhen
  {
    _eventTime  :: EPCISTime
  , _recordTime :: Maybe EPCISTime -- minOccurs = 0
  , _timeZone   :: TimeZone
  }
  deriving (Show, Eq, Generic)

makeClassy ''DWhen

-- |eventTime, recordTime, the timezone is eventTime's
mkDWhen :: String -> String -> Maybe DWhen
mkDWhen a b = let et = parseStr2Time a :: Either EPCISTimeError EPCISTime
                  rt = parseStr2Time b :: Either EPCISTimeError EPCISTime
                  tz = parseStr2TimeZone a :: Either EPCISTimeError TimeZone in
                  case et of
                    Right et' -> case rt of
                                   Right rt' -> let diff = diffUTCTime et' rt' in
                                                    if diff < 0
                                                    then Just $ DWhen et' (Just rt') (fromRight' tz) else Nothing
                                   _         -> Just $ DWhen et' Nothing (fromRight' tz)
                    _         -> Nothing

-- |use it when event time and record time are the same
mkDWhen' :: String -> Maybe DWhen
mkDWhen' s = let t = parseStr2Time s :: Either EPCISTimeError EPCISTime
                 tz = parseStr2TimeZone s :: Either EPCISTimeError TimeZone in
                 case t of
                   Right t' -> Just $ DWhen t' (Just t') (fromRight' tz)
                   _        -> Nothing
