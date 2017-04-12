{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeOperators   #-}


module Data.GS1.EventID where

import           Control.Lens
import           Data.UUID
import           Data.Aeson
import           Data.Aeson.TH
import           GHC.Generics

newtype EventID = EventID UUID
  deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions ''UUID)
$(deriveJSON defaultOptions ''EventID)

makeClassy ''EventID
