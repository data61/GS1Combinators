{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | module containing error types, URI class, epc types
-- the types in this file cover all dimensions

module Data.GS1.EPC
  ( URI
  , ParseFailure(..)
  , XMLSnippet(..)
  , MissingTag(..)
  , EventIdStr(..)
  , GS1CompanyPrefix(..)
  , ItemReference(..)
  , ExtensionDigit(..)
  , SerialReference(..)
  , CheckDigit(..)
  , Lot(..)
  , IndividualAssetReference(..)
  , SerialNumber(..)
  , SGLNExtension(..)
  , Uom(..)
  , Amount(..)
  , AssetType(..)
  , Lng(..)
  , Lat(..)
  , LocationReference(..)
  , DocumentType(..)
  , ServiceReference(..)
  , BizTransactionId(..)
  , TransformationId(..)
  , EPCISTime(..)
  , SGTINFilterValue(..)
  , Quantity(..)
  , InstanceLabelEPC(..)
  , ClassLabelEPC(..)
  , LocationEPC(..)
  , SourceDestType(..)
  , BusinessTransactionEPC(..)
  , LocationError(..)
  , BizStep(..)
  , BizTransactionType(..)
  , BizTransaction(..)
  , Action(..)
  , Disposition(..)
  , readURI
  , renderURL
  , readURIClassLabelEPC
  , readURIInstanceLabelEPC
  , mkAction
  )
  where

import           Control.Lens    hiding ((.=))
import           Data.Aeson      as A
import           Data.Swagger
import qualified Data.Text       as T
import           GHC.Generics    (Generic)
import           Web.HttpApiData

import           Data.Bifunctor  (first)
import           Data.GS1.Utils
import           Data.Time
import           Data.UUID       (UUID)

import           Data.Hashable   (Hashable (..))

newtype XMLSnippet = XMLSnippet T.Text deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
newtype MissingTag = MissingTag T.Text deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
newtype EventIdStr = EventIdStr T.Text deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

-- add more type values to this if need be
data ParseFailure
  = InvalidLength XMLSnippet
  -- ^ Length is not correct
  | InvalidFormat XMLSnippet
  -- ^ Components Missing, incorrectly structured, wrong payload
  | InvalidAction XMLSnippet
  -- ^ When parsing an action failed
  | InvalidBizTransaction XMLSnippet
  -- ^ When parsing a bizTransaction failed
  | InvalidEventId EventIdStr
  -- ^ When the ``eventId`` in the XML is not a valid UUID
  | TimeZoneError XMLSnippet
  -- ^ Error in parsing timezone
  | TagNotFound MissingTag
  -- ^ When a mandatory tag is not found
  | MultipleTags T.Text
  -- ^ When more than the specified number of tags are present
  | InvalidDispBizCombination XMLSnippet
  -- ^ When the disposition does not go with the bizstep
  | ChildFailure [ParseFailure]
  -- ^ When there is a list of Parsefailures
  -- typically applicable to higher level structures,
  -- like DWhat, DWhere, etc
  deriving (Show, Read, Eq, Generic, ToJSON)


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

-- makeInvalidLength :: [T.Text] -> Either ParseFailure a
makeErrorType :: (XMLSnippet -> ParseFailure) -> [T.Text] -> Either ParseFailure b
makeErrorType e snippets = Left $ e (XMLSnippet $ dots snippets)


-- |Assigned by a GS1 Member Organisation to a user/subscriber
newtype GS1CompanyPrefix = GS1CompanyPrefix {unGS1CompanyPrefix :: T.Text}
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
instance ToParamSchema GS1CompanyPrefix
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
-- $(deriveJSON defaultOptions ''SGTINFilterValue)
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

-- | A quantity can be either the amount and the unit of measurement,
-- or the item count.
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
-- $(deriveJSON defaultOptions ''Quantity)
instance ToSchema Quantity

instance FromJSON Quantity where
  parseJSON = withObject "Quantity" $ \o -> do
    uom <- o .:? "uom"
    case uom of
      Nothing -> ItemCount <$> o .: "quantity"
      Just uom' -> do
        q <- o .: "quantity"
        pure $ MeasuredQuantity (Amount q) uom'

instance ToJSON Quantity where
  toJSON (MeasuredQuantity a b) = object [ "quantity" .= a
                                         , "uom" .= b
                                         ]
  toJSON (ItemCount a) = object [ "quantity" .= a ]

-- Given a suffix/uri body, returns a list of strings separated by "."
-- The separator should be passed on as an argument to this function in order
-- to make it more generalised
getSuffixTokens :: [T.Text] -> [T.Text]
getSuffixTokens suffix = T.splitOn "." $ T.concat suffix

--GS1_EPC_TDS_i1_10.pdf (page 27)
data ClassLabelEPC
  -- | LGTIN = GTIN + Batch/Lot scheme is used to denote a class of objects
  -- belonging to a given batch or lot of a given GTIN
  = LGTIN
    { _lgtinCompanyPrefix :: GS1CompanyPrefix
    , _lgtinItemReference :: ItemReference
    , _lgtinLot           :: Lot
    }
    -- | Class SGTIN.
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

-- implement reader for :idpat:sgtin:
readURIClassLabelEPC :: [T.Text] -> Either ParseFailure ClassLabelEPC
readURIClassLabelEPC ("urn" : "epc" : "class" : "lgtin" : rest) =
  Right $ LGTIN (GS1CompanyPrefix pfix) (ItemReference itemReference) (Lot lot)
    where [pfix, itemReference, lot] = getSuffixTokens rest
readURIClassLabelEPC ("urn" : "epc" : "idpat" : "sgtin" : rest) =
  Right $ CSGTIN (GS1CompanyPrefix pfix) Nothing (ItemReference itemReference)
    where (pfix:itemReference:_) = getSuffixTokens rest
readURIClassLabelEPC xSnippet = makeErrorType InvalidFormat xSnippet

instance FromJSON ClassLabelEPC where
  parseJSON = withText "ClassLabelEPC" (either (fail . show) pure . readURI)

instance ToJSON ClassLabelEPC where
  toJSON = String . renderURL

instance ToSchema ClassLabelEPC


data InstanceLabelEPC
  -- | Global Individual Asset Identifier,
  --  e.g. bucket for olives
  = GIAI
    {
      _giaiCompanyPrefix :: GS1CompanyPrefix
    , _giaiSerialNum     :: SerialNumber
    }
  -- | Serial Shipping Container Code
  | SSCC
    {
      _ssccCompanyPrefix :: GS1CompanyPrefix
    , _ssccSerialNum     :: SerialNumber
    }
  -- | Serialsed Global Trade Item Number
  | SGTIN
    {
      _sgtinCompanyPrefix    :: GS1CompanyPrefix
    , _sgtinSgtinFilterValue :: Maybe SGTINFilterValue
    , _sgtinItemReference    :: ItemReference
    , _sgtinSerialNum        :: SerialNumber
    }
  -- | Global Returnable Asset Identifier
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

-- | GS1_EPC_TDS_i1_11.pdf Page 28
sgtinPadLen :: Int
sgtinPadLen = 13

-- | GS1_EPC_TDS_i1_11.pdf Page 29
ssccPadLen :: Int
ssccPadLen = 17


-- TODO: This could be easily implemnted using proper parser combinators from attoparsec
-- parsec, megaparsec or trifecta (parsers library)
readURIInstanceLabelEPC :: [T.Text] -> Either ParseFailure InstanceLabelEPC
readURIInstanceLabelEPC ("urn" : "epc" : "id" : "giai" : rest) =
  Right $ GIAI (GS1CompanyPrefix pfix) (SerialNumber sn)
    where [pfix, sn] = getSuffixTokens rest

readURIInstanceLabelEPC xSnippet@("urn" : "epc" : "id" : "sscc" : rest)
  | isCorrectLen = Right $ SSCC (GS1CompanyPrefix pfix) (SerialNumber sn)
  | otherwise = makeErrorType InvalidLength xSnippet
      where
        [pfix, sn] = getSuffixTokens rest
        isCorrectLen =
            getTotalLength [pfix, sn] == ssccPadLen

readURIInstanceLabelEPC ("urn" : "epc" : "id" : "grai" : rest) =
  Right $ GRAI (GS1CompanyPrefix pfix) (AssetType assetType) (SerialNumber sn)
    where [pfix, assetType, sn] = getSuffixTokens rest

readURIInstanceLabelEPC xSnippet@("urn" : "epc" : "id" : "sgtin" : rest)
  | isCorrectLen =
      Right $ SGTIN (GS1CompanyPrefix pfix) Nothing (ItemReference ir) (SerialNumber sn)
                                         -- Nothing, for the moment
  | otherwise = makeErrorType InvalidLength xSnippet
      where
        [pfix, ir, sn] = getSuffixTokens rest
        isCorrectLen =
            getTotalLength [pfix, ir] == sgtinPadLen

readURIInstanceLabelEPC xSnippet = makeErrorType InvalidFormat xSnippet

instance FromJSON InstanceLabelEPC where
  parseJSON = withText "InstanceLabelEPC" (either (fail . show) pure . readURI)

instance ToJSON InstanceLabelEPC where
  toJSON = String . renderURL

-- $(deriveJSON defaultOptions ''InstanceLabelEPC)
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
-- $(deriveJSON defaultOptions ''LocationReference)

-- | EPCIS_Guideline.pdf Release 1.2 Ratified Feb 2017 - page 35 4.7.2
-- In EPCIS, the GLN+extension is represented as a Uniform Resource Identifier
-- (URI) according to the EPC Tag Data Standard. The specific type of URI is
-- called an SGLN, which is capable of representing either a
-- GLN+extension or a GLN without extension. The SGLN for
-- GLN 0614141111114 and extension 987 looks like this:
-- urn:epc:id:sgln:0614141.11111.978
data LocationEPC = SGLN {
    _sglnCompanyPrefix :: GS1CompanyPrefix
  , _locationRef       :: LocationReference
  , _sglnExt           :: Maybe SGLNExtension
  }
  deriving (Show, Read, Eq, Generic)

instance ToSchema LocationReference

instance Hashable LocationEPC where
  hashWithSalt salt (SGLN pfx _ _) = hashWithSalt salt $ unGS1CompanyPrefix pfx

instance URI LocationEPC where
  uriPrefix SGLN{} = "urn:epc:id:sgln:"
  uriSuffix (SGLN (GS1CompanyPrefix pfix) (LocationReference loc) (Just (SGLNExtension ext))) =
    Right [pfix, loc, ext]
  uriSuffix (SGLN (GS1CompanyPrefix pfix) (LocationReference loc) Nothing) = Right [pfix, loc]

  readURI epcStr
   | isLocationEPC (T.splitOn ":" epcStr) =
      readURILocationEPC $ T.splitOn "." $ last $ T.splitOn ":" epcStr -- TODO: Last is unsafe
   | otherwise            = Left $ InvalidFormat (XMLSnippet epcStr)


instance FromJSON LocationEPC where
  parseJSON = withText "LocationEPC" (either (fail . show) pure . readURI)

instance ToJSON LocationEPC where
  toJSON = String . renderURL

isLocationEPC :: [T.Text] -> Bool
isLocationEPC ("urn" : "epc" : "id" : "sgln" : _) = True
isLocationEPC _                                   = False

-- | GS1_EPC_TDS_i1_11.pdf Page 29
sglnPadLen :: Int
sglnPadLen = 12

getExt :: T.Text -> Maybe SGLNExtension
getExt "0" = Nothing
getExt s   = Just (SGLNExtension s)

readURILocationEPC :: [T.Text] -> Either ParseFailure LocationEPC
-- without extension
readURILocationEPC xSnippet@[pfix, loc]
  | isCorrectLen =
      Right $ SGLN (GS1CompanyPrefix pfix) (LocationReference loc) Nothing
  | otherwise    = makeErrorType InvalidLength xSnippet
    where
      isCorrectLen = getTotalLength [pfix, loc] == sglnPadLen

-- with extension
readURILocationEPC xSnippet@([pfix, loc, extNum])
  | isCorrectLen =
      Right $
        SGLN (GS1CompanyPrefix pfix) (LocationReference loc) (getExt extNum)
  | otherwise    = makeErrorType InvalidLength xSnippet
    where
      isCorrectLen = getTotalLength [pfix, loc] == sglnPadLen

readURILocationEPC xSnippet =  makeErrorType InvalidFormat xSnippet
-- error condition / invalid input

instance ToSchema LocationEPC
-- | EPCIS_Guideline.pdf Release 1.2 Ratified Feb 2017 - page 19
-- Source List and Destination List: is used to provide additional business context
-- when an EPCIS event is part of a business transfer of ownership, responsibility
-- or custody. As with business transactions, a source or destination is identified by
-- a pair of identifiers: the type of the source or destination and an identifier
-- of the source or destination of that type. The GS1 CBV (section 7.4.2)
-- distinguishes three standard source/destination types: “owning_party”,
-- “possessing_party”, “location”.
data SourceDestType
  = SDOwningParty
  | SDPossessingParty
  | SDLocation
  deriving (Show, Eq, Generic, Read)
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
readSrcDestURI errTxt             = Left $ InvalidFormat (XMLSnippet errTxt)

instance FromJSON SourceDestType where
  parseJSON = withText "SourceDestType" (either (fail . show) pure . readURI)

instance ToJSON SourceDestType where
  toJSON = String . renderURL

-- https://github.csiro.au/Blockchain/GS1Combinators/blob/master/doc/GS1_EPC_TDS_i1_11.pdf
newtype DocumentType     = DocumentType {unDocumentType :: T.Text}
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
newtype ServiceReference = ServiceReference {unServiceReference :: T.Text}
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
instance ToSchema DocumentType


--TODO: Implement URI instances for GCN and CPID

-- |  Other GS1 object identifiers include GDTI for documents,
-- GIAI for individual assets, GRAI for returnable assets,
-- GSRN for services, GCN for coupons, and CPID for components or parts.
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
readURIBusinessTransactionEPC xSnippet@([pfix, sref])
  | isCorrectLen = Right $ GSRN (GS1CompanyPrefix pfix) (SerialReference sref)
  | otherwise = makeErrorType InvalidLength xSnippet
  where
    isCorrectLen =
        getTotalLength [pfix, sref] == gsrnPadLen
readURIBusinessTransactionEPC xSnippet@([pfix, docType, sn])
  | isCorrectLen = Right $ GDTI (GS1CompanyPrefix pfix) (DocumentType docType) (SerialNumber sn) -- BUG!
  | otherwise = makeErrorType InvalidLength xSnippet
  where
    isCorrectLen = getTotalLength [pfix, docType, sn] == gdtiPadLen
readURIBusinessTransactionEPC xSnippet = makeErrorType InvalidFormat xSnippet

-- $(deriveJSON defaultOptions ''BusinessTransactionEPC)
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
instance ToSchema BizStep

instance FromJSON BizStep where
  parseJSON = withText "BizStep" $ \t ->
    case parseURI t "urn:epcglobal:cbv:bizstep" of
      Just bizstep -> pure bizstep
      Nothing      -> fail "Invalid Bizstep"

instance ToJSON BizStep where
  toJSON = String . renderURL

ppBizStep :: BizStep -> T.Text
ppBizStep = revertCamelCase . T.pack . show

readURIBizStep :: Maybe BizStep -> T.Text -> Either ParseFailure BizStep
readURIBizStep Nothing        s = Left $ InvalidFormat (XMLSnippet s)
readURIBizStep (Just bizstep) _ = Right bizstep

-- CBV-Standard-1-2-r-2016-09-29.pdf page 16
instance URI BizStep where
  uriPrefix _ = "urn:epcglobal:cbv:bizstep:"
  uriSuffix = Left . ppBizStep
  readURI s = let pURI = parseURI s "urn:epcglobal:cbv:bizstep" :: Maybe BizStep
              in readURIBizStep pURI s

{-
  Example:

  <bizTransactionList>
    <bizTransaction type="urn:epcglobal:cbv:btt:po">
      http://transaction.acme.com/po/12345678
    </bizTransaction>
  </bizTransactionList>

-}


newtype BizTransactionId = BizTransactionId {unBizTransactionId :: T.Text}
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
instance ToSchema BizTransactionId

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
-- $(deriveJSON defaultOptions ''BizTransactionType)
instance ToSchema BizTransactionType

ppBizTransactionType :: BizTransactionType -> T.Text
ppBizTransactionType = revertCamelCase . T.pack . show

readURIBizTransactionType :: Maybe BizTransactionType
                          -> T.Text
                          -> Either ParseFailure BizTransactionType
readURIBizTransactionType Nothing    s = Left $ InvalidFormat (XMLSnippet s)
readURIBizTransactionType (Just btt) _ = Right btt

-- CBV-Standard-1-2-r-2016-09-29.pdf page 28
instance URI BizTransactionType where
  uriPrefix _ = "urn:epcglobal:cbv:btt:"
  uriSuffix = Left . ppBizTransactionType
  readURI s = let pURI = parseURI s "urn:epcglobal:cbv:btt" :: Maybe BizTransactionType
              in readURIBizTransactionType pURI s

instance FromJSON BizTransactionType where
  parseJSON = withText "BizTransactionType" (either (fail . show) pure . readURI)

instance ToJSON BizTransactionType where
  toJSON = String . renderURL

-- |BizTransaction CBV Section 7.3 and Section 8.5
data BizTransaction = BizTransaction
  {
    _btid :: Maybe BizTransactionId
  , _bt   :: BizTransactionType
  }
  deriving (Show, Eq, Generic)
-- $(deriveJSON defaultOptions ''BizTransaction)
instance ToSchema BizTransaction

instance FromJSON BizTransaction where
  parseJSON = withObject "BizTransaction" $ \o -> BizTransaction
      <$> o .:? "type"
      <*> o .: "bizTransaction"

instance ToJSON BizTransaction where
  toJSON (BizTransaction mbId tr) =
    object $ [ "bizTransaction" A..= tr ]
            <> optionally "type" mbId

-- | TransformationId
-- From the spec EPCIS-Standard-1.2-r-2016-09-29.pdf Page 55
-- Some transformation business processes take place over a long period of time, and so it is more
-- appropriate to represent them as a series of EPCIS events. A TransfomationId may be included
-- in two or more TransformationEvents to link them together. When events share an identical
-- TransformationId, the meaning is that the inputs to any of those events may have contributed in
-- some way to each of the outputs in any of those same events.
newtype TransformationId = TransformationId {unTransformationId :: UUID}
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
instance ToSchema TransformationId

data Action
  = Add
  | Observe
  | Delete
  deriving (Show, Eq, Generic, Read)
-- $(deriveJSON defaultOptions ''Action)
instance ToSchema Action
instance ToParamSchema Action where
  toParamSchema _ = mempty
    & type_ .~ SwaggerString
instance FromHttpApiData Action where
  parseQueryParam t = first (T.pack . show) (mkAction t)

instance FromJSON Action where
  parseJSON = withText "Action" $ \case
    "ADD" -> pure Add
    "OBSERVE" -> pure Observe
    "DELETE" -> pure Delete
    t -> fail $ "Invalid value for Action: " <> T.unpack t

instance ToJSON Action where
  toJSON Add     = "ADD"
  toJSON Observe = "OBSERVE"
  toJSON Delete  = "DELETE"

mkAction :: T.Text -> Either ParseFailure Action
mkAction t =
  case mkByName . camelCase $ T.toLower t of
    Nothing -> Left $ InvalidAction (XMLSnippet t)
    Just x  -> Right x

---------------------------
-- WHY  -------------------
---------------------------

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
instance ToSchema Disposition

ppDisposition :: Disposition -> T.Text
ppDisposition = revertCamelCase . T.pack . show

-- CBV-Standard-1-2-r-2016-09-29.pdf page 24
readURIDisposition :: Maybe Disposition -> T.Text -> Either ParseFailure Disposition
readURIDisposition Nothing     s = Left $ InvalidFormat (XMLSnippet s)
readURIDisposition (Just disp) _ = Right disp

instance URI Disposition where
  uriPrefix _ = "urn:epcglobal:cbv:disp:"
  uriSuffix = Left . ppDisposition
  readURI  s    = let pURI = parseURI s "urn:epcglobal:cbv:disp" :: Maybe Disposition
                    in readURIDisposition pURI s

instance FromJSON Disposition where
  parseJSON = withText "Disposition" (either (fail . show) pure . readURI)

instance ToJSON Disposition where
  toJSON = String . renderURL

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
  deriving (Show, Read, Eq, Generic, Ord)

instance ToSchema EPCISTime

instance ToJSON EPCISTime where
  toJSON = String . T.pack . formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S.%sZ")) . unEPCISTime
instance FromJSON EPCISTime where
  parseJSON = fmap EPCISTime . parseJSON

