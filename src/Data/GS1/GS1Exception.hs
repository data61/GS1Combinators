module Data.GS1.GS1Exception where

import           Control.Exception
import           Data.Typeable

-- |Type GLNException
data GLNException = InvalidGLNLengthException String
  deriving (Show, Typeable)

instance Exception GLNException

-- |Type EPCISTimeException
data EPCISTimeException = InvalidDateException String | InvalidTimeException String
  deriving (Show, Typeable)

instance Exception EPCISTimeException
