{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.GS1.EPCISTime where

import           Data.Time
import           GHC.Generics
import           Control.Lens.TH

{-
   A timestamp, giving the date and time in a time zone-independent manner.
   For bindings in which fields of this type are represented textually,
   an ISO-8601 compliant representation SHOULD be used.
-}
-- |The TimeZone will be saved independently
type EPCISTime = UTCTime

data EPCISTimeError = IllegalTimeFormat deriving (Show, Eq, Generic)

makeClassyPrisms ''EPCISTimeError
