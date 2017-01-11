{-# LANGUAGE TemplateHaskell #-}
module Data.GS1.EventID where

import           Control.Lens
import           Data.UUID

data EventID = EventID UUID
  deriving (Show, Eq)

makeClassy ''EventID
