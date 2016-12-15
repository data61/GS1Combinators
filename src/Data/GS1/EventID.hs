module Data.GS1.EventID where

import           Data.UUID

data EventID = EventID UUID
  deriving (Show, Eq)
