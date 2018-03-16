{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
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

import           Data.Monoid

-- add more type values to this if need be
data ParseFailure
  = InvalidLength
  -- ^ Length is not correct
  -- CHECK in Disposition, InvalidFormat can also indicate wrong payload... FIXME?
  | InvalidFormat
  -- ^ Components Missing, incorrectly structured
  | InvalidAction
  -- ^ When parsing an action failed
  | InvalidBizTransaction
  -- ^ When parsing a bizTransaction failed
  | InvalidEvent
  -- ^ When parsing an event failed
  | TimeZoneError
  -- ^ error in parsing timezone
  | TagNotFound
  -- ^ when a mandatory tag is not found
  | InvalidDispBizCombination
  -- ^ when the disposition does not go with the bizstep
  | ChildFailure [ParseFailure]
  -- ^ when there is a list of Parsefailures
  -- typically applicable to higher level structures,
  -- like DWhat, DWhere, etc

  -- TODO: Make this type a Semigroup/Monoid so we can use the Validation type
  deriving (Show, Eq)

-- |Anything that could be converted into URI

{- TODO: This class could be improved with the vollowing change:

class URI a where
  uriPrefix :: Proxy a -> T.Text
  -- Produce a list of components to be joined by dots, or a custom text value
  uriComponents :: a -> Either T.Text [T.Text]
  readURI  :: T.Text -> Either ParseFailure a

renderURI :: URI a -> a -> T.Text
renderURI a = uriPrefix (Proxy :: Proxy a) <> either id dots (uriComponents a)

which would remove a lot of the redundant code in this module

-}
class URI a where
  printURI :: a -> T.Text
  readURI  :: T.Text -> Either ParseFailure a


-- |Assigned by a GS1 Member Organisation to a user/subscriber
newtype GS1CompanyPrefix  = GS1CompanyPrefix T.Text                deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
newtype ItemReference     = ItemReference T.Text                   deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
newtype ExtensionDigit    = ExtensionDigit Int                     deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
newtype SerialReference   = SerialReference T.Text                 deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
-- |Calculated according to an algorithm https://en.wikipedia.org/wiki/Global_Location_Number
newtype CheckDigit               = CheckDigit Int                  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
newtype Lot                      = Lot T.Text                      deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
newtype IndividualAssetReference = IndividualAssetReference T.Text deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
newtype SerialNumber             = SerialNumber T.Text             deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
newtype SGLNExtension            = SGLNExtension T.Text            deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

instance ToSchema GS1CompanyPrefix
instance ToSchema ItemReference
instance ToSchema ExtensionDigit
instance ToSchema SerialReference
instance ToSchema CheckDigit
instance ToSchema Lot
instance ToSchema IndividualAssetReference
instance ToSchema SerialNumber
instance ToSchema SGLNExtension


dots :: [T.Text] -> T.Text
dots = T.intercalate "."


data SGTINFilterValue
  =  AllOthers
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

newtype Uom       = Uom T.Text       deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
newtype Amount    = Amount Float     deriving (Show, Read, Eq, Generic, FromJSON, ToJSON) -- TODO: Float is almost never the right type
newtype AssetType = AssetType T.Text deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

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
    {
      _lgtinCompanyPrefix :: GS1CompanyPrefix
    , _lgtinItemReference :: ItemReference
    , _lgtinLot           :: Lot
    }
    -- e.g. olives in a vat, harvested in April 2017
  | CSGTIN
    {
      _csgtinCompanyPrefix    :: GS1CompanyPrefix
    , _csgtinSgtinFilterValue :: Maybe SGTINFilterValue
    , _csgtinItemReference    :: ItemReference
    }
    deriving (Show, Read, Eq, Generic)

instance URI ClassLabelEPC where
    printURI = printURIClassLabelEPC
    readURI epcStr = readURIClassLabelEPC $ T.splitOn ":" epcStr

-- move GRAI to InstanceLabel
-- implement reader for :idpat:sgtin:
readURIClassLabelEPC :: [T.Text] -> Either ParseFailure ClassLabelEPC
readURIClassLabelEPC ("urn" : "epc" : "class" : "lgtin" : rest) =
  Right $ LGTIN (GS1CompanyPrefix gs1CompanyPrefix) (ItemReference itemReference) (Lot lot)
    where [gs1CompanyPrefix, itemReference, lot] = getSuffixTokens rest
readURIClassLabelEPC ("urn" : "epc" : "idpat" : "sgtin" : rest) =
  Right $ CSGTIN (GS1CompanyPrefix gs1CompanyPrefix) Nothing (ItemReference itemReference)
    where (gs1CompanyPrefix:itemReference:_) = getSuffixTokens rest
readURIClassLabelEPC _ = Left InvalidFormat

printURIClassLabelEPC :: ClassLabelEPC -> T.Text
printURIClassLabelEPC (LGTIN (GS1CompanyPrefix pfix) (ItemReference ir) (Lot lot)) =
  "urn:epc:class:lgtin:" <> dots [pfix, ir, lot]

printURIClassLabelEPC (CSGTIN (GS1CompanyPrefix pref) _ (ItemReference ir)) =
  "urn:epc:idpat:sgtin:" <> dots [pref, ir]

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
    printURI = printURIInstanceLabelEPC
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
            -- TODO: BUG? no sn in check?

readURIInstanceLabelEPC _ = Left InvalidFormat


printURIInstanceLabelEPC :: InstanceLabelEPC -> T.Text
printURIInstanceLabelEPC (GIAI (GS1CompanyPrefix pfix) (SerialNumber sn)) = -- TODO: SerialNumber was previously individualAssetReference????
  "urn:epc:id:giai:" <> dots [pfix, sn]
printURIInstanceLabelEPC (SSCC (GS1CompanyPrefix pfix) (SerialNumber sn)) =
  "urn:epc:id:sscc:" <> dots [pfix, sn]
printURIInstanceLabelEPC (SGTIN (GS1CompanyPrefix pfix) _ (ItemReference ref) (SerialNumber sn)) =
  "urn:epc:id:sgtin:" <> dots [pfix, ref, sn]
printURIInstanceLabelEPC (GRAI (GS1CompanyPrefix pfix) (AssetType aType) (SerialNumber sn)) =
  "urn:epc:id:grai:" <> dots [pfix, aType, sn]

$(deriveJSON defaultOptions ''InstanceLabelEPC)
instance ToSchema InstanceLabelEPC

newtype Lng = Lng Double deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
newtype Lat = Lat Double deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
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

-- |non-normative representation - simplest form of RFC5870
-- deprecated, kept momentarily for reference
-- ppLocationReference :: LocationReference -> String
-- ppLocationReference (LocationCoord lat lng) = printf "%f,%f" lat lng -- new standard
-- ppLocationReference (LocationReference str) = str

instance ToSchema LocationReference

instance URI LocationEPC where
  printURI (SGLN (GS1CompanyPrefix pfix) (LocationReference lr) (Just (SGLNExtension ext))) =
    "urn:epc:id:sgln:" <> dots [pfix, lr, ext]
  printURI (SGLN (GS1CompanyPrefix pfix) (LocationReference lr) Nothing) =
    "urn:epc:id:sgln:" <> dots [pfix, lr]

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

data SourceDestType = SDOwningParty
                    | SDPossessingParty
                    | SDLocation
                    deriving (Show, Eq, Generic, Read)
$(deriveJSON defaultOptions ''SourceDestType)
instance ToSchema SourceDestType

instance URI SourceDestType where
  printURI = printSrcDestURI
  readURI epc = readSrcDestURI $ last $ T.splitOn ":" epc

srcDestPrefixStr :: T.Text
srcDestPrefixStr = "urn:epcglobal:cbv:sdt:"

printSrcDestURI :: SourceDestType -> T.Text
printSrcDestURI SDOwningParty = T.append srcDestPrefixStr "owning_party"
printSrcDestURI SDPossessingParty = T.append srcDestPrefixStr "possessing_party"
printSrcDestURI SDLocation = T.append srcDestPrefixStr "location"

readSrcDestURI :: T.Text -> Either ParseFailure SourceDestType
readSrcDestURI "owning_party"     = Right SDOwningParty
readSrcDestURI "possessing_party" = Right SDPossessingParty
readSrcDestURI "location"         = Right SDLocation
readSrcDestURI _                  = Left InvalidFormat

-- https://github.csiro.au/Blockchain/GS1Combinators/blob/master/doc/GS1_EPC_TDS_i1_11.pdf
newtype DocumentType     = DocumentType T.Text     deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
newtype ServiceReference = ServiceReference T.Text deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
instance ToSchema DocumentType

data BusinessTransactionEPC =  GDTI {
                                 _gdtiCompanyPrefix :: GS1CompanyPrefix
                               , _gdtiDocType       :: DocumentType
                               , _gdtiSerialNum     :: SerialNumber
                               }
                             | GSRN {
                                 _gsrnCompanyPrefix :: GS1CompanyPrefix
                               , _gsrnSerialRef     :: SerialReference
                               }
                              deriving (Show, Read, Eq, Generic)

-- urn:epc:id:gdti:CompanyPrefix.DocumentType.SerialNumber
instance URI BusinessTransactionEPC where
  printURI = printURIBizTransactionEPC
  readURI epcStr = readURIBusinessTransactionEPC $
                      getSuffixTokens [last $ T.splitOn ":" epcStr]
--                    Getting the uri body out of the string

printURIBizTransactionEPC :: BusinessTransactionEPC -> T.Text
printURIBizTransactionEPC (GDTI (GS1CompanyPrefix pfix) (DocumentType docType) (SerialNumber sn)) =
  "urn:epc:id:gsrn:" <> dots [pfix, docType, sn]
printURIBizTransactionEPC (GSRN (GS1CompanyPrefix pfix) (SerialReference sref)) =
  "urn:epc:id:gsrn:" <> dots [pfix, sref]

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


-- |Allocated by the company to a specific location
newtype LocationRef = LocationRef T.Text deriving (Show, Read, Eq, Generic)


data LocationError
  = IllegalGLNFormat
  | InvalidChecksum
  deriving (Show, Eq, Generic)


---------------------------
-- WHAT -------------------
---------------------------

-- CBV-Standard-1-2-r-2016-09-29.pdf Page 17
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
  printURI epc = T.append bizstepPrefixStr (ppBizStep epc)
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


newtype BizTransactionID = BizTransactionID T.Text deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
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
  printURI btt  = T.append "urn:epcglobal:cbv:btt:" (ppBizTransactionType btt)
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
newtype TransformationID = TransformationID UUID deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
instance ToSchema TransformationID

data Action = Add
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
  -- Is this a different format from the rest of the URI instances?
  printURI disp = T.append "urn:epcglobal:cbv:disp:" (ppDisposition disp)
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
newtype EPCISTime = EPCISTime UTCTime deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
instance ToSchema EPCISTime

-- copied from https://github.com/data61/bom-solar-webservice/blob/master/app/Main.hs
-- | A datatype representing a UTCTime shown and read using the ISO 8601 format with HH:MM:SS and timezone
--
{-
newtype ISO8601 = ISO8601 UTCTime deriving (Eq, Generic, ToJSON, FromJSON)
iso8601Format = iso8601DateFormat $ Just "%H:%M:%S%z"
instance Show            ISO8601 where show (ISO8601 t) = formatTime defaultTimeLocale iso8601Format t
instance Read            ISO8601 where readsPrec p = (coerce :: ReadS UTCTime -> ReadS ISO8601) $ readSTime True defaultTimeLocale iso8601Format
instance ToParamSchema   ISO8601 where
  toParamSchema _ = mempty
    & type_ .~ SwaggerString
    & format .~ Just (pack iso8601Format)
-}

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

-- DELETED ErrorReasonID since incorrect since it
-- -- |EPCIS 1.2 section 7.5
-- -- FIXME example should be found to verify the implementation is correct
-- data ErrorReasonID = DidNotOccur
--                    | IncorrectData
--                    deriving (Show, Eq, Generic)

-- ppErrorReasonID :: ErrorReasonID -> String
-- ppErrorReasonID = revertCamelCase . show

-- data ErrorDeclaration = ErrorDeclaration
--   {
--     _declarationTime    :: EPCISTime
--   , _reason             :: Maybe ErrorReasonID
--   , _correctiveEventIDs :: Maybe [EventID]
--   }
--   deriving (Show, Eq, Generic)


-- ppErrorDecleration  (ErrorDeclaration _ r _) = case r of
--                                           Just a  -> ppErrorReasonID a
--                                           Nothing -> ""

-- instance URI ErrorDeclaration where
--   printURI er  = "urn:epcglobal:cbv:er:" ++ ppErrorDecleration er
--   readURI _    = undefined --FIXME

{-
-- |calculate the check digit from gs1company prefix and location reference
--  https://data61.slack.com/files/zzhu/F35T5N1L0/check_digit_calculator.pdf
calcCheckDigit :: GS1CompanyPrefix -> LocationRef -> Int
calcCheckDigit pref ref = getDigit (map digitToInt (pref ++ ref)) where
  getDigit arr = 10 - (sumDigit arr `mod` 10)
    where sumDigit arr2 = case arr2 of
                          (a:b:xs) -> a + b * 3 + sumDigit xs
                          _        -> 0

-- | Check the length and isDigit for all chars
wellFormatGLN :: GS1CompanyPrefix -> LocationRef -> CheckDigit -> Bool
wellFormatGLN pref ref cd = _concat pref ref == 12 && length cd == 1 && _isNum pref ref cd
  where _concat a b = length (a ++ b)
        _isNum a b c =
          let mint = readMaybe (concat [a, b, c]) :: Maybe Integer in
          case mint of
            Just _  -> True
            Nothing -> False

-- |validateGLN
validateGLN :: GS1CompanyPrefix -> LocationRef -> CheckDigit -> Bool
validateGLN pref ref cd = calcCheckDigit pref ref == (read cd::Int)

commented out because EPCs are now a class, not a type.
-- |Creates a valid GLN
gln ::(AsLocationError e, MonadError e m)
     => GS1CompanyPrefix -> LocationRef -> CheckDigit -> m EPC
gln pref ref cd
  | not (wellFormatGLN pref ref cd) = throwing _IllegalGLNFormat ()
  | not (validateGLN pref ref cd)   = throwing _InvalidChecksum ()
  | otherwise                       = pure (GLN pref ref cd)

-- |type -> payload -> Maybe EPC
-- TODO: add more types
mkEPC :: String -> String -> Maybe EPC
mkEPC t p = case t of
              "EPC" -> Just $ EPC p
              "GLN" -> case splitOn "." p of
                         [a, b, c] -> let x = gln a b c :: Either LocationError EPC in
                                          if isRight x then Just (fromRight' x) else Nothing
                         _         -> Nothing
              _     -> Nothing
-}
