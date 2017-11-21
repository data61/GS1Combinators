{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.GS1.Object where

import           Codec.Binary.UTF8.String
import           Data.Attoparsec.ByteString (maybeResult, parse)
import qualified Data.ByteString.Char8      as C
import           Data.Char                  (toLower)
import           Data.GS1.EPC
import           Network.Parser.Rfc3986     (segmentNz)
import           Data.Aeson
import           Data.Aeson.TH
import           GHC.Generics
import           Data.Swagger

-- |TODO expand it to the proper implementation when necessary
-- EPCIS Page 29
--type Quantity = Double

--type Uom = String

-- |Simple quantity representation
data QuantityElement = QuantityElement EPCClass Quantity (Maybe Uom)
  deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''QuantityElement)
instance ToSchema QuantityElement

-- |Alias of QuantityList
type QuantityList = [QuantityElement]

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
privateObjectID :: String -> Maybe ObjectID
privateObjectID = undefined

-- CBV 8.2.3
-- RFC 3986 segment nz
-- get the ObjectID from Http link, a colon might be required
httpObjectID :: String -> Maybe ObjectID
httpObjectID s =
  let result = maybeResult $ parse segmentNz (C.pack s)
   in
    fmap (Codec.Binary.UTF8.String.decode) result


hexDigit :: Char -> Maybe Char
hexDigit c = if toLower c `elem` (['0'..'9'] ++ ['a'..'f']) then Just c else Nothing

pctEncoded' :: Char -> Char -> Maybe String
pctEncoded' a b = form <$> hexDigit a <*> hexDigit b where
  form a' b' = ['%', a', b']

-- XXX you probably want to pattern here instead of using length
isPctEncoded :: String -> Bool
isPctEncoded s
  | length s == 3 = isPctEncoded' s
  | otherwise     = False
  where
    isPctEncoded' s' = case s' of
                         -- XXX these three lines can be rewritten using the _Just prism and the function has (an excellent first exercise!)
                         [x, a, b] -> case x of
                                        '%' -> let ec = pctEncoded' a b in
                                                   case ec of
                                                     Just _  -> True
                                                     Nothing -> False
                                        _   -> False
                         _         -> False


unreserved :: String
unreserved = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-._~"

subdelims :: String
subdelims = "!$&'()*+,;="

validSegmentNzChar :: Char -> Bool
validSegmentNzChar c = c `elem` ['@', ':'] ++ unreserved ++ subdelims

validateObjectID :: String -> Maybe ObjectID
validateObjectID s = case s of
                       ""    -> Nothing
                       (_:_) -> oidHelp s
                         where
                           oidHelp s' = case s' of
                                          ""     -> Just ""
                                          (x:xs) -> case x of
                                                      '%' -> case xs of
                                                               (a:b:xxs) -> if isPctEncoded [x, a, b]
                                                                               then (++) <$> Just [x, a, b] <*> oidHelp xxs
                                                                               else Nothing
                                                               _         -> Nothing
                                                      _   -> if validSegmentNzChar x
                                                                 then (:) <$> Just x <*> oidHelp xs
                                                                 else Nothing

data IDLevel = InstanceLevelT
             | ClassLevelT
             deriving (Eq, Show, Generic)
$(deriveJSON defaultOptions ''IDLevel)
instance ToSchema IDLevel

data ObjectType = PhysicalT
                | DigitalT
                deriving (Eq, Show, Generic)
$(deriveJSON defaultOptions ''ObjectType)
instance ToSchema ObjectType

-- |EPCIS 1.0
data Object = Object IDLevel ObjectType ObjectID
  deriving (Eq, Show, Generic)
$(deriveJSON defaultOptions ''Data.GS1.Object.Object)
instance ToSchema Data.GS1.Object.Object

-- TODO ObjectClassID
-- class level identifier
type ObjectClassID = String
