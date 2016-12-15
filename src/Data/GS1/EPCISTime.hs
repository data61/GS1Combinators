module Data.GS1.EPCISTime where

import           Data.Time.Clock

{-
   A timestamp, giving the date and time in a time zone-independent manner.
   For bindings in which fields of this type are represented textually,
   an ISO-8601 compliant representation SHOULD be used.
-}
type EPCISTime = UniversalTime
