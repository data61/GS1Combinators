{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | module containing error types, URI class, epc types
-- the types in this file cover all dimensions

module Data.GS1.EPC where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Swagger
import qualified Data.Text       as T
import           GHC.Generics
import           Web.HttpApiData

import           Data.Bifunctor  (first)
import           Data.GS1.Utils
import           Data.Time
import           Data.UUID       (UUID)

-- import           Data.Monoid     hiding ((<>))
import           Data.Semigroup

-- add more type values to this if need be
data ParseFailure
  = InvalidLength
  -- ^ Length is not correct
  | InvalidFormat
  -- ^ Components Missing, incorrectly structured, wrong payload
  | InvalidAction
  -- ^ When parsing an action failed
  | InvalidBizTransaction
  -- ^ When parsing a bizTransaction failed
  | InvalidEvent
  -- ^ When parsing an event failed
  | TimeZoneError
  -- ^ Error in parsing timezone
  | TagNotFound
  -- ^ When a mandatory tag is not found
  | InvalidDispBizCombination
  -- ^ When the disposition does not go with the bizstep
  | ChildFailure [ParseFailure]
  -- ^ When there is a list of Parsefailures
  -- typically applicable to higher level structures,
  -- like DWhat, DWhere, etc
  deriving (Show, Eq)

instance Semigroup ParseFailure where
  ChildFailure xs <> ChildFailure ys = ChildFailure (xs++ys)
  ChildFailure [] <> y               = y -- Needed for mempty <> x = x law
  x <> ChildFailure []               = x
  ChildFailure xs <> y               = ChildFailure (xs++[y])
  x <> ChildFailure ys               = ChildFailure (x:ys)
  x <> y                             = ChildFailure [x,y]

instance Monoid ParseFailure where
  mempty = ChildFailure []
  mappend = (<>)

-- |Anything that could be converted into URI

class URI a where
  {-# MINIMAL uriPrefix, uriSuffix, readURI #-}
  uriPrefix      :: a -> T.Text
  uriSuffix      :: a -> Either T.Text [T.Text]
  readURI        :: T.Text -> Either ParseFailure a

  -- | Should not be directly implemented unless the format for URIs differs
  -- substantially from other URIs
  renderURL :: a -> T.Text
  renderURL a = uriPrefix a <> either id dots (uriSuffix a)

dots :: [T.Text] -> T.Text
dots = T.intercalate "."


-- |Assigned by a GS1 Member Organisation to a user/subscriber
newtype GS1CompanyPrefix  = GS1CompanyPrefix {unGS1CompanyPrefix :: T.Text}
  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
newtype ItemReference     = ItemReference {unItemReference :: T.Text}
  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
newtype ExtensionDigit    = ExtensionDigit {unExtensionDigit :: Int}
  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
newtype SerialReference   = SerialReference {unSerialReference :: T.Text}
  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

-- |Calculated according to an algorithm https://en.wikipedia.org/wiki/Global_Location_Number
newtype CheckDigit               = CheckDigit {unCheckDigit :: Int}
  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
newtype Lot                      = Lot {unLot :: T.Text}
  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
newtype IndividualAssetReference =
  IndividualAssetReference {unIndividualAssetReference :: T.Text}
    deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
newtype SerialNumber             = SerialNumber {unSerialNumber :: T.Text}
  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
newtype SGLNExtension            = SGLNExtension {unSGLNExtension :: T.Text}
  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

instance ToSchema GS1CompanyPrefix
instance ToSchema ItemReference
instance ToSchema ExtensionDigit
instance ToSchema SerialReference
instance ToSchema CheckDigit
instance ToSchema Lot
instance ToSchema IndividualAssetReference
instance ToSchema SerialNumber
instance ToSchema SGLNExtension

data SGTINFilterValue
  = AllOthers
  | POSTradeItem
  | FullCaseForTransport
  | Reserved1
  | InnerPackTradeItemGroupingForHandling
  | Reserved2
  | UnitLoad
  | UnitInsideTradeItemOrComponentInsideAProductNotIntendedForIndividualSale
  deriving (Eq, Generic, Read, Enum, Show)
$(deriveJSON defaultOptions ''SGTINFilterValue)
instance ToSchema SGTINFilterValue

{-
■ The GS1 Company Prefix, assigned by GS1 to a managing entity.
  This is the same as the GS1 Company Prefix digits within a GS1 SSCC key.

■ The Serial Reference, assigned by the managing entity to a particular
  logistics handling unit. The Serial Reference as it appears
  in the EPC URI is derived from the SSCC by concatenating the
  Extension Digit of the SSCC and the Serial Reference digits,
  and treating the result as a single numeric string.
-}

newtype Uom       = Uom {unUom :: T.Text}
  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
newtype Amount    = Amount {unAmount :: Double}
  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
newtype AssetType = AssetType {unAssetType :: T.Text}
  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

instance ToSchema Amount
instance ToSchema Uom
instance ToSchema AssetType

data Quantity
  = MeasuredQuantity
    {
      _quantityAmount :: Amount
    , _quantityUom    :: Uom
    }
  | ItemCount
    {
      _quantityCount :: Integer
    }
    deriving (Show, Read, Eq, Generic)
$(deriveJSON defaultOptions ''Quantity)
instance ToSchema Quantity

-- Given a suffix/uri body, returns a list of strings separated by "."
-- The separator should be passed on as an argument to this function in order
-- to make it more generalised
getSuffixTokens :: [T.Text] -> [T.Text]
getSuffixTokens suffix = T.splitOn "." $ T.concat suffix

--GS1_EPC_TDS_i1_10.pdf (page 27)
data ClassLabelEPC
  = LGTIN
    { _lgtinCompanyPrefix :: GS1CompanyPrefix
    , _lgtinItemReference :: ItemReference
    , _lgtinLot           :: Lot
    }
    -- e.g. olives in a vat, harvested in April 2017
  | CSGTIN
    { _csgtinCompanyPrefix    :: GS1CompanyPrefix
    , _csgtinSgtinFilterValue :: Maybe SGTINFilterValue
    , _csgtinItemReference    :: ItemReference
    }
    deriving (Show, Read, Eq, Generic)

instance URI ClassLabelEPC where
  uriPrefix LGTIN{}  = "urn:epc:class:lgtin:"
  uriPrefix CSGTIN{} = "urn:epc:idpat:sgtin:"

  uriSuffix
    (LGTIN (GS1CompanyPrefix pfix) (ItemReference itemReference) (Lot lot)) =
      Right [pfix, itemReference, lot]
  uriSuffix
    (CSGTIN (GS1CompanyPrefix pfix) _ (ItemReference itemReference)) =
      Right [pfix, itemReference]

  readURI epcStr = readURIClassLabelEPC $ T.splitOn ":" epcStr


-- move GRAI to InstanceLabel
-- implement reader for :idpat:sgtin:
readURIClassLabelEPC :: [T.Text] -> Either ParseFailure ClassLabelEPC
readURIClassLabelEPC ("urn" : "epc" : "class" : "lgtin" : rest) =
  Right $ LGTIN (GS1CompanyPrefix pfix) (ItemReference itemReference) (Lot lot)
    where [pfix, itemReference, lot] = getSuffixTokens rest
readURIClassLabelEPC ("urn" : "epc" : "idpat" : "sgtin" : rest) =
  Right $ CSGTIN (GS1CompanyPrefix pfix) Nothing (ItemReference itemReference)
    where (pfix:itemReference:_) = getSuffixTokens rest
readURIClassLabelEPC _ = Left InvalidFormat


$(deriveJSON defaultOptions ''ClassLabelEPC)
instance ToSchema ClassLabelEPC


data InstanceLabelEPC
  -- | Global Individual Asset Identifier,
  -- e.g. bucket for olives
  = GIAI
    {
      _giaiCompanyPrefix :: GS1CompanyPrefix
    , _giaiSerialNum     :: SerialNumber
    }
  -- | serial shipping container code
  | SSCC
    {
      _ssccCompanyPrefix :: GS1CompanyPrefix
    , _ssccSerialNum     :: SerialNumber
    }
  -- | serialsed global trade item number
  | SGTIN
    {
      _sgtinCompanyPrefix    :: GS1CompanyPrefix
    , _sgtinSgtinFilterValue :: Maybe SGTINFilterValue
    , _sgtinItemReference    :: ItemReference
    , _sgtinSerialNum        :: SerialNumber
    }
  -- | Global returnable asset identifier
  | GRAI
    {
      _graiCompanyPrefix :: GS1CompanyPrefix
    , _graiAssetType     :: AssetType
    , _graiSerialNum     :: SerialNumber
    }
  deriving (Show, Read, Eq, Generic)

instance URI InstanceLabelEPC where
  uriPrefix GIAI{}  = "urn:epc:id:giai:"
  uriPrefix SSCC{}  = "urn:epc:id:sscc:"
  uriPrefix SGTIN{} = "urn:epc:id:sgtin:"
  uriPrefix GRAI{}  = "urn:epc:id:grai:"

  uriSuffix (GIAI (GS1CompanyPrefix pfix) (SerialNumber sn)) = Right [pfix, sn]
  uriSuffix (SSCC (GS1CompanyPrefix pfix) (SerialNumber sn)) = Right [pfix, sn]
  -- TODO: Should the second argument be used?
  uriSuffix (SGTIN (GS1CompanyPrefix pfix) _ (ItemReference ir) (SerialNumber sn)) =
    Right [pfix, ir, sn]
  uriSuffix (GRAI (GS1CompanyPrefix pfix) (AssetType aType) (SerialNumber sn)) =
    Right [pfix, aType, sn]

  readURI epcStr = readURIInstanceLabelEPC $ T.splitOn ":" epcStr

-- GS1_EPC_TDS_i1_11.pdf Page 28
sgtinPadLen :: Int
sgtinPadLen = 13

-- GS1_EPC_TDS_i1_11.pdf Page 29
ssccPadLen :: Int
ssccPadLen = 17


-- TODO: This could be easily implemnted using proper parser combinators from attoparsec
-- parsec, megaparsec or trifecta (parsers library)
readURIInstanceLabelEPC :: [T.Text] -> Either ParseFailure InstanceLabelEPC

readURIInstanceLabelEPC ("urn" : "epc" : "id" : "giai" : rest) =
  Right $ GIAI (GS1CompanyPrefix pfix) (SerialNumber sn)
    where [pfix, sn] = getSuffixTokens rest

readURIInstanceLabelEPC ("urn" : "epc" : "id" : "sscc" : rest)
  | isCorrectLen = Right $ SSCC (GS1CompanyPrefix pfix) (SerialNumber sn)
  | otherwise = Left InvalidLength
      where
        [pfix, sn] = getSuffixTokens rest
        isCorrectLen =
            getTotalLength [pfix, sn] == ssccPadLen

readURIInstanceLabelEPC ("urn" : "epc" : "id" : "grai" : rest) =
  Right $ GRAI (GS1CompanyPrefix pfix) (AssetType assetType) (SerialNumber sn)
    where [pfix, assetType, sn] = getSuffixTokens rest

readURIInstanceLabelEPC ("urn" : "epc" : "id" : "sgtin" : rest)
  | isCorrectLen =
      Right $ SGTIN (GS1CompanyPrefix pfix) Nothing (ItemReference ir) (SerialNumber sn)
                                         -- Nothing, for the moment
  | otherwise = Left InvalidLength
      where
        [pfix, ir, sn] = getSuffixTokens rest
        isCorrectLen =
            getTotalLength [pfix, ir] == sgtinPadLen

readURIInstanceLabelEPC _ = Left InvalidFormat


$(deriveJSON defaultOptions ''InstanceLabelEPC)
instance ToSchema InstanceLabelEPC

newtype Lng = Lng {unLng :: Double}
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
newtype Lat = Lat {unLat :: Double}
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
newtype LocationReference
  = LocationReference
  {
    _locationRefVal :: T.Text
  }
  deriving (Read, Eq, Generic, Show)
$(deriveJSON defaultOptions ''LocationReference)


data LocationEPC = SGLN {
    _sglnCompanyPrefix :: GS1CompanyPrefix
  , _locationRef       :: LocationReference
  , _sglnExt           :: Maybe SGLNExtension
  }
  deriving (Show, Read, Eq, Generic)
$(deriveJSON defaultOptions ''LocationEPC)

instance ToSchema LocationReference

instance URI LocationEPC where
  uriPrefix SGLN{} = "urn:epc:id:sgln:"
  uriSuffix (SGLN (GS1CompanyPrefix pfix) (LocationReference loc) (Just (SGLNExtension ext))) =
    Right [pfix, loc, ext]
  uriSuffix (SGLN (GS1CompanyPrefix pfix) (LocationReference loc) Nothing) = Right [pfix, loc]

  readURI epcStr
   | isLocationEPC (T.splitOn ":" epcStr) =
      readURILocationEPC $ T.splitOn "." $ last $ T.splitOn ":" epcStr -- TODO: Last is unsafe
   | otherwise            = Left InvalidFormat

isLocationEPC :: [T.Text] -> Bool
isLocationEPC ("urn" : "epc" : "id" : "sgln" : _) = True
isLocationEPC _                                   = False

-- GS1_EPC_TDS_i1_11.pdf Page 29
sglnPadLen :: Int
sglnPadLen = 12

getExt :: T.Text -> Maybe SGLNExtension
getExt "0" = Nothing
getExt s   = Just (SGLNExtension s)

readURILocationEPC :: [T.Text] -> Either ParseFailure LocationEPC
-- without extension
readURILocationEPC [pfix, loc]
  | isCorrectLen =
      Right $ SGLN (GS1CompanyPrefix pfix) (LocationReference loc) Nothing
  | otherwise    = Left InvalidLength
    where
      isCorrectLen = getTotalLength [pfix, loc] == sglnPadLen

-- with extension
readURILocationEPC [pfix, loc, extNum]
  | isCorrectLen =
      Right $
        SGLN (GS1CompanyPrefix pfix) (LocationReference loc) (getExt extNum)
  | otherwise    = Left InvalidLength
    where
      isCorrectLen = getTotalLength [pfix, loc] == sglnPadLen

readURILocationEPC _ = Left InvalidFormat -- error condition / invalid input

instance ToSchema LocationEPC

data SourceDestType
  = SDOwningParty
  | SDPossessingParty
  | SDLocation
  deriving (Show, Eq, Generic, Read)
$(deriveJSON defaultOptions ''SourceDestType)
instance ToSchema SourceDestType

instance URI SourceDestType where
  uriPrefix _ = "urn:epcglobal:cbv:sdt:"
  uriSuffix SDOwningParty     = Left "owning_party"
  uriSuffix SDPossessingParty = Left "possessing_party"
  uriSuffix SDLocation        = Left "location"
  readURI epc = readSrcDestURI $ last $ T.splitOn ":" epc


readSrcDestURI :: T.Text -> Either ParseFailure SourceDestType
readSrcDestURI "owning_party"     = Right SDOwningParty
readSrcDestURI "possessing_party" = Right SDPossessingParty
readSrcDestURI "location"         = Right SDLocation
readSrcDestURI _                  = Left InvalidFormat

-- https://github.csiro.au/Blockchain/GS1Combinators/blob/master/doc/GS1_EPC_TDS_i1_11.pdf
newtype DocumentType     = DocumentType {unDocumentType :: T.Text}
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
newtype ServiceReference = ServiceReference {unServiceReference :: T.Text}
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
instance ToSchema DocumentType

data BusinessTransactionEPC
  = GDTI {
    _gdtiCompanyPrefix :: GS1CompanyPrefix
  , _gdtiDocType       :: DocumentType
  , _gdtiSerialNum     :: SerialNumber }
  | GSRN {
    _gsrnCompanyPrefix :: GS1CompanyPrefix
  , _gsrnSerialRef     :: SerialReference }
  deriving (Show, Read, Eq, Generic)

-- urn:epc:id:gdti:CompanyPrefix.DocumentType.SerialNumber
instance URI BusinessTransactionEPC where
  uriPrefix GDTI{} = "urn:epc:id:gsrn:"
  uriPrefix GSRN{} = "urn:epc:id:gsrn:"

  uriSuffix (GDTI (GS1CompanyPrefix pfix) (DocumentType documentType) (SerialNumber sn)) =
    Right [pfix, documentType, sn]
  uriSuffix (GSRN (GS1CompanyPrefix pfix) (SerialReference sr)) = Right [pfix, sr]

  readURI epcStr = readURIBusinessTransactionEPC $
                      getSuffixTokens [last $ T.splitOn ":" epcStr]
--                    Getting the uri body out of the string


-- the length of the arguments should equal to the following,
-- according to the spec
-- used for the purposes of validation

-- GS1_EPC_TDS_i1_11.pdf Page 31
gsrnPadLen :: Int
gsrnPadLen = 17

-- GS1_EPC_TDS_i1_11.pdf Page 32
gdtiPadLen :: Int
gdtiPadLen = 12

readURIBusinessTransactionEPC :: [T.Text] ->
                                  Either ParseFailure BusinessTransactionEPC
readURIBusinessTransactionEPC [pfix, sref]
  | isCorrectLen = Right $ GSRN (GS1CompanyPrefix pfix) (SerialReference sref)
  | otherwise = Left InvalidLength
  where
    isCorrectLen =
        getTotalLength [pfix, sref] == gsrnPadLen
readURIBusinessTransactionEPC [pfix, docType, sn]
  | isCorrectLen = Right $ GDTI (GS1CompanyPrefix pfix) (DocumentType docType) (SerialNumber sn) -- BUG!
  | otherwise = Left InvalidLength
  where
    isCorrectLen =
        getTotalLength [pfix, docType, sn] ==
          gdtiPadLen
readURIBusinessTransactionEPC _ = Left InvalidFormat

$(deriveJSON defaultOptions ''BusinessTransactionEPC)
instance ToSchema BusinessTransactionEPC

data LocationError
  = IllegalGLNFormat
  | InvalidChecksum
  deriving (Show, Eq, Generic)

-- | CBV-Standard-1-2-r-2016-09-29.pdf Page 17
data BizStep
  = Accepting
  | Arriving
  | Assembling
  | Collecting
  | Commissioning
  | Consigning
  | CreatingClassInstance
  | CycleCounting
  | Decommissioning
  | Departing
  | Destroying
  | Disassembling
  | Dispensing
  | Encoding
  | EnteringExiting
  | Holding
  | Inspecting
  | Installing
  | Killing
  | Loading
  | Other
  | Packing
  | Picking
  | Receiving
  | Removing
  | Repackaging
  | Repairing
  | Replacing
  | Reserving
  | RetailSelling
  | Shipping
  | StagingOutbound
  | StockTaking
  | Stocking
  | Storing
  | Transporting
  | Unloading
  | VoidShipping
  deriving (Show, Eq, Generic, Read)
$(deriveJSON defaultOptions ''BizStep)
instance ToSchema BizStep

ppBizStep :: BizStep -> T.Text
ppBizStep = revertCamelCase . T.pack . show

bizstepPrefixStr :: T.Text
bizstepPrefixStr = "urn:epcglobal:cbv:bizstep:"

readURIBizStep :: Maybe BizStep -> Either ParseFailure BizStep
readURIBizStep Nothing        = Left InvalidFormat
readURIBizStep (Just bizstep) = Right bizstep

-- CBV-Standard-1-2-r-2016-09-29.pdf page 16
instance URI BizStep where
  uriPrefix _ = "urn:epcglobal:cbv:bizstep:"
  uriSuffix = Left . ppBizStep
  readURI  s   = let pURI = parseURI s "urn:epcglobal:cbv:bizstep" :: Maybe BizStep
                   in readURIBizStep pURI

{-
  Example:

  <bizTransactionList>
    <bizTransaction type="urn:epcglobal:cbv:btt:po">
      http://transaction.acme.com/po/12345678
    </bizTransaction>
  </bizTransactionList>

-}


newtype BizTransactionID = BizTransactionID {unBizTransactionID :: T.Text}
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
instance ToSchema BizTransactionID

data BizTransactionType
  = Bol       -- Bill of Lading
  | Desadv    -- Dispatch Advice
  | Inv       -- Invoice
  | Pedigree  -- Pedigree
  | Po        -- Purchase Order
  | Poc       -- Purchase Order Confirmation
  | Prodorder -- Production Order
  | Recadv    -- Receiving Advice
  | Rma       -- Return Mechandise Authorisation
  deriving (Show, Eq, Generic, Read)
$(deriveJSON defaultOptions ''BizTransactionType)
instance ToSchema BizTransactionType

ppBizTransactionType :: BizTransactionType -> T.Text
ppBizTransactionType = revertCamelCase . T.pack . show

readURIBizTransactionType :: Maybe BizTransactionType ->
                              Either ParseFailure BizTransactionType
readURIBizTransactionType Nothing    = Left InvalidFormat
readURIBizTransactionType (Just btt) = Right btt

-- CBV-Standard-1-2-r-2016-09-29.pdf page 28
instance URI BizTransactionType where
  uriPrefix _ = "urn:epcglobal:cbv:btt:"
  uriSuffix = Left . ppBizTransactionType
  readURI s    = let pURI = parseURI s "urn:epcglobal:cbv:btt" :: Maybe BizTransactionType
                      in readURIBizTransactionType pURI

-- |BizTransaction CBV Section 7.3 and Section 8.5
data BizTransaction = BizTransaction
  {
    _btid :: BizTransactionID
  , _bt   :: BizTransactionType
  }
  deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions ''BizTransaction)
instance ToSchema BizTransaction


-- | TransformationID
-- From the spec EPCIS-Standard-1.2-r-2016-09-29.pdf Page 55
-- Some transformation business processes take place over a long period of time, and so it is more
-- appropriate to represent them as a series of EPCIS events. A TransfomationID may be included
-- in two or more TransformationEvents to link them together. When events share an identical
-- TransformationID, the meaning is that the inputs to any of those events may have contributed in
-- some way to each of the outputs in any of those same events.
newtype TransformationID = TransformationID {unTransformationID :: UUID}
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
instance ToSchema TransformationID

data Action
  = Add
  | Observe
  | Delete
  deriving (Show, Eq, Generic, Read)
$(deriveJSON defaultOptions ''Action)
instance ToSchema Action
instance ToParamSchema Action where
  toParamSchema _ = mempty
    & type_ .~ SwaggerString
instance FromHttpApiData Action where
  parseQueryParam t = first (T.pack . show) (mkAction t)

mkAction :: T.Text -> Either ParseFailure Action
mkAction t =
  case mkByName . camelCase $ T.toLower t of
    Nothing -> Left InvalidAction
    Just x  -> Right x

---------------------------
-- WHY  -------------------
---------------------------

data DispositionError
  = InvalidDisposition
  | OtherDispositionError
  deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions ''DispositionError)
instance ToSchema DispositionError


data Disposition
  = Active
  | ContainerClosed
  | Damaged
  | Destroyed
  | Dispensed
  | Disposed
  | Encoded
  | Expired
  | InProgress
  | InTransit
  | Inactive
  | NoPedigreeMatch
  | NonSellableOther
  | PartiallyDispensed
  | Recalled
  | Reserved
  | RetailSold
  | Returned
  | SellableAccessible
  | SellableNotAccessible
  | Stolen
  | Unknown
  deriving (Show, Eq, Generic, Read)
$(deriveJSON defaultOptions ''Disposition)
instance ToSchema Disposition


ppDisposition :: Disposition -> T.Text
ppDisposition = revertCamelCase . T.pack . show

-- CBV-Standard-1-2-r-2016-09-29.pdf page 24
readURIDisposition :: Maybe Disposition -> Either ParseFailure Disposition
readURIDisposition Nothing     = Left InvalidFormat
readURIDisposition (Just disp) = Right disp

instance URI Disposition where
  uriPrefix _ = "urn:epcglobal:cbv:disp:"
  uriSuffix = Left . ppDisposition
  readURI  s    = let pURI = parseURI s "urn:epcglobal:cbv:disp" :: Maybe Disposition
                    in readURIDisposition pURI

---------------------------
-- WHEN  -------------------
---------------------------
{-
   A timestamp, giving the date and time in a time zone-independent manner.
   For bindings in which fields of this type are represented textually,
   an ISO-8601 compliant representation SHOULD be used.
-}
-- |The TimeZone will be saved independently
newtype EPCISTime = EPCISTime {unEPCISTime :: UTCTime}
  deriving (Show, Read, Eq, Generic, Ord, ToJSON, FromJSON)
instance ToSchema EPCISTime

data EPCISTimeError = IllegalTimeFormat deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions ''EPCISTimeError)
instance ToSchema EPCISTimeError


instance Eq ZonedTime where
  x == y = show x == show y

$(deriveJSON defaultOptions ''TimeZone)
--instance ToSchema ZonedTime
instance ToParamSchema TimeZone where
  toParamSchema _ = mempty
    & type_ .~ SwaggerString

-- copied from
-- https://hackage.haskell.org/package/swagger2-2.1.3/docs/src/Data.Swagger.Internal.Schema.html#line-477
named :: T.Text -> Schema -> NamedSchema
named n = NamedSchema (Just n) -- this function has been Eta reduced

timeSchema :: T.Text -> Schema
timeSchema fmt = mempty
  & type_ .~ SwaggerString
  & format ?~ fmt


-- XXX I have literally no idea what is happening here! Please check!
instance ToSchema TimeZone where
  declareNamedSchema _ = pure $ named (T.pack "TimeZone") $ timeSchema (T.pack "date-time")
