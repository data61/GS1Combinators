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
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Swagger
import qualified Data.Text as T

import Database.SQLite.Simple.ToField
import Data.Aeson.Text
import Data.ByteString.Char8 (pack)
import qualified Data.Text.Lazy as TxtL
{-
   A timestamp, giving the date and time in a time zone-independent manner.
   For bindings in which fields of this type are represented textually,
   an ISO-8601 compliant representation SHOULD be used.
-}
-- |The TimeZone will be saved independently
type EPCISTime = UTCTime

-- copied from https://github.com/data61/bom-solar-webservice/blob/master/app/Main.hs
-- | A datatype representing a UTCTime shown and read using the ISO 8601 format with HH:MM:SS and timezone
--
{-
newtype ISO8601 = ISO8601 UTCTime deriving (Eq, Generic, ToJSON, FromJSON)
iso8601Format = iso8601DateFormat $ Just "%H:%M:%S%z"
instance Show            ISO8601 where show (ISO8601 t) = formatTime defaultTimeLocale iso8601Format t
instance Read            ISO8601 where readsPrec p = (coerce :: ReadS UTCTime -> ReadS ISO8601) $ readSTime True defaultTimeLocale iso8601Format
instance ToParamSchema   ISO8601 where
  toParamSchema _ = mempty
    & type_ .~ SwaggerString
    & format .~ Just (pack iso8601Format)
-}

data EPCISTimeError = IllegalTimeFormat deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions ''EPCISTimeError)
instance ToSchema EPCISTimeError

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

instance Eq ZonedTime where
  x==y = (show x) == (show y)
data DWhen = DWhen
  {
    _eventTime  :: EPCISTime
  , _recordTime :: Maybe EPCISTime-- minOccurs = 0
  , _timeZone   :: TimeZone
  }
  deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions ''TimeZone)
--instance ToSchema ZonedTime
instance ToParamSchema TimeZone where
  toParamSchema _ = mempty
    & type_ .~ SwaggerString

instance ToField TimeZone where
  toField = toField . pack . show

-- copied from
-- https://hackage.haskell.org/package/swagger2-2.1.3/docs/src/Data.Swagger.Internal.Schema.html#line-477
named :: T.Text -> Schema -> NamedSchema
named name schema = NamedSchema (Just name) schema

timeSchema :: T.Text -> Schema
timeSchema fmt = mempty
  & type_ .~ SwaggerString
  & format ?~ fmt


-- XXX I have literally no idea what is happening here! Please check!
instance ToSchema TimeZone where
  declareNamedSchema _ = pure $ named (T.pack "TimeZone") $ timeSchema (T.pack "date-time")

$(deriveJSON defaultOptions ''DWhen)
instance ToSchema DWhen

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
