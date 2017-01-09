module Data.GS1.Object where

import           Codec.Binary.UTF8.String
import           Data.Attoparsec.ByteString
import qualified Data.ByteString.Char8      as C
import           Data.GS1.EPC
import           Network.Parser.Rfc3986

-- |TODO expand it to the proper implementation when necessary
-- EPCIS Page 29
type Quantity = Integer

type Uom = String

-- |Simple quantity representation
data QuantityElement = QuantityElement EPCClass Quantity Uom
  deriving (Show, Eq)

-- |EPCIS 7.3.6
-- Not sure if it is right way
-- ILMD is data that describes a specific instance of a physical or digital
-- object, or a specific batch/lot of objects that are produced in batches/lot.
type Ilmd = [String]

-- |The ObjectID
-- |Ref: CBV 8.2 & 8.3, EPCIS 1.0
type ObjectID = String

-- CBV 8.2.2
-- RFC 2141
-- no suitable 2141 package yet, the urn package is tested with ghc 7.6
privateObjectID :: String -> Maybe objectID
privateObjectID = undefined

-- CBV 8.2.3
-- RFC 3986 segment nz
-- get the ObjectID from Http link, a colon might be required
httpObjectID :: String -> Maybe ObjectID
httpObjectID s = let result = maybeResult $ parse segmentNz (C.pack s) in
                     case result of
                       Just wa -> Just $ decode wa
                       Nothing -> Nothing


data IDLevel = InstanceLevelT
             | ClassLevelT
             deriving (Eq, Show)

-- |EPCIS 1.0
data Object = Object IDLevel ObjectID
  deriving (Eq, Show)

-- FIXME The Object should be redesigned to aviod confusion

-- TODO ObjectClassID
