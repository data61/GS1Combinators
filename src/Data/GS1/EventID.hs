{-# LANGUAGE TemplateHaskell #-}
module Data.GS1.EventID where

import           Control.Lens
import           Data.UUID

newtype EventID = EventID UUID
  deriving (Show, Eq)

makeClassy ''EventID
