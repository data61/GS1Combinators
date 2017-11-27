{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.GS1.EPC where

import           Control.Lens.TH
import           Control.Monad.Error.Lens
import           Control.Monad.Except     (MonadError)
import           Control.Lens
import           Data.Char
import           Data.Either.Combinators
import           GHC.Generics
import qualified Data.Text as T
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Swagger
import           Text.Printf
import           Data.List.Split
import           Data.Maybe
import           Data.List

import           Data.Time
import           Data.ByteString.Char8 (pack)
import           Data.GS1.EventID
import           Data.GS1.Utils

import           Database.SQLite.Simple.ToField

-- More Refernce: TDS 1.9

-- URI Prefix
type URIPrefix = String

-- URI Quantifier
type URIQuantifier = String

-- URI Payload
type URIPayload = String

-- |Anything that could be converted into URI
class URI a where
  printURI      :: a -> String
  readURI       :: String -> Maybe a

-- |Assigned by a GS1 Member Organisation to a user/subscriber
type GS1CompanyPrefix = String
type ItemReference = String
type ExtensionDigit = Int
type SerialReference = String
-- |Calculated according to an algorithm https://en.wikipedia.org/wiki/Global_Location_Number
type CheckDigit = Int
type Lot = String
type IndividualAssetReference = String
type SerialNumber = String
type SGLNExtension = String

data SGTINFilterValue =  AllOthers
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

type Uom = String
type Amount = Float
type AssetType = String

data Quantity =   MeasuredQuantity Amount Uom
                | ItemCount Integer
                deriving (Show, Read, Eq, Generic)
$(deriveJSON defaultOptions ''Quantity)
instance ToSchema Quantity


-- Given a suffix/uri body, returns a list of strings separated by "."
-- The separator should be passed on as an argument to this function in order
-- to make it more generalised
getSuffixTokens :: [String] -> [String]
getSuffixTokens suffix = splitOn "." $ concat suffix


--GS1_EPC_TDS_i1_10.pdf (page 27)
data ClassLabelEPC = LGTIN GS1CompanyPrefix ItemReference Lot
                     -- e.g. olives in a vat, harvested in April 2017
                    |GRAI GS1CompanyPrefix AssetType SerialNumber
                     --Global returnable asset identifier
                     deriving (Show, Read, Eq, Generic)

instance URI ClassLabelEPC where
    printURI = printURIClassLabelEPC
    readURI epcStr = readURIClassLabelEPC $ splitOn ":" epcStr
  
instance ToField ClassLabelEPC where
  toField = toField . pack . show


readURIClassLabelEPC :: [String] -> Maybe ClassLabelEPC
readURIClassLabelEPC ("urn" : "epc" : "class" : "lgtin" : rest) =
  Just $ LGTIN gs1CompanyPrefix itemReference lot
    where [gs1CompanyPrefix, itemReference, lot] = getSuffixTokens rest
readURIClassLabelEPC ("urn" : "epc" : "id" : "grai" : rest) = -- TODO - 
  Just $ GRAI gs1CompanyPrefix assetType serialNumber
    where [gs1CompanyPrefix, assetType, serialNumber] = getSuffixTokens rest
-- readURIClassLabelEPC _ = error "Invalid Label string or type not implemented yet"
readURIClassLabelEPC _ = Nothing


printURIClassLabelEPC :: ClassLabelEPC -> String
printURIClassLabelEPC (LGTIN gs1CompanyPrefix itemReference lot) =
  "urn:epc:class:lgtin:" ++ gs1CompanyPrefix ++ "." ++ itemReference ++ "." ++ lot
printURIClassLabelEPC (GRAI gs1CompanyPrefix assetType serialNumber) =
  "urn:epc:id:grai:" ++ gs1CompanyPrefix ++ "." ++ assetType ++ "." ++ serialNumber

$(deriveJSON defaultOptions ''ClassLabelEPC)
instance ToSchema ClassLabelEPC


data InstanceLabelEPC = GIAI GS1CompanyPrefix SerialNumber 
                       -- Global Individual Asset Identifier, e.g. bucket for olives
                       |SSCC GS1CompanyPrefix SerialNumber --serial shipping container code
                       |SGTIN GS1CompanyPrefix (Maybe SGTINFilterValue) ItemReference SerialNumber
                       --serialsed global trade item number
                       deriving (Show, Read, Eq, Generic)


instance URI InstanceLabelEPC where
    printURI = printURIInstanceLabelEPC
    readURI epcStr = readURIInstanceLabelEPC $ splitOn ":" epcStr


readURIInstanceLabelEPC :: [String] -> Maybe InstanceLabelEPC
readURIInstanceLabelEPC ("urn" : "epc" : "id" : "giai" : rest) =
  Just $ GIAI gs1CompanyPrefix individualAssetReference
    where [gs1CompanyPrefix, individualAssetReference] = getSuffixTokens rest
readURIInstanceLabelEPC ("urn" : "epc" : "id" : "sscc" : rest) =
  Just $ SSCC gs1CompanyPrefix serialNumber
    where [gs1CompanyPrefix, serialNumber] = getSuffixTokens rest
readURIInstanceLabelEPC ("urn" : "epc" : "id" : "sgtin" : rest) =
  Just $ SGTIN gs1CompanyPrefix Nothing itemReference serialNumber -- Nothing, for the moment
    where [gs1CompanyPrefix, itemReference, serialNumber] = getSuffixTokens rest
-- readURIInstanceLabelEPC _ = error "Invalid Label string or type not implemented yet"
readURIInstanceLabelEPC _ = Nothing


printURIInstanceLabelEPC :: InstanceLabelEPC -> String
printURIInstanceLabelEPC (GIAI gs1CompanyPrefix individualAssetReference) =
  "urn:epc:id:giai:" ++ gs1CompanyPrefix ++ "." ++ individualAssetReference
printURIInstanceLabelEPC (SSCC gs1CompanyPrefix serialNumber) =
  "urn:epc:id:sscc:" ++ gs1CompanyPrefix ++ "." ++ serialNumber
printURIInstanceLabelEPC (SGTIN gs1CompanyPrefix (Just sgtinFilterValue) itemReference serialNumber) =
  "urn:epc:id:sgtin:" ++ gs1CompanyPrefix ++ "." ++ itemReference ++ "." ++ serialNumber
    --FIXME: add Maybe SGTINFilterValue
printURIInstanceLabelEPC (SGTIN gs1CompanyPrefix Nothing itemReference serialNumber) =
  "urn:epc:id:sgtin:" ++ gs1CompanyPrefix ++ "." ++ itemReference ++ "." ++ serialNumber
    --FIXME: add Maybe SGTINFilterValue


$(deriveJSON defaultOptions ''InstanceLabelEPC)
instance ToSchema InstanceLabelEPC

instance ToField InstanceLabelEPC where
    toField = toField . pack . show


-- this should be moved to src/.../DWhat.hs
data LabelEPC = CL ClassLabelEPC (Maybe Quantity) | IL InstanceLabelEPC
                deriving (Show, Read, Eq, Generic)

$(deriveJSON defaultOptions ''LabelEPC)
instance ToSchema LabelEPC


type Lng = Float
type Lat = Float
data LocationReference = LocationCoord Lat Lng
                        |LocationReferenceNum String
  deriving (Read, Eq, Generic)


data LocationEPC = SGLN GS1CompanyPrefix LocationReference (Maybe SGLNExtension)
  deriving (Show, Read, Eq, Generic)

-- |non-normative representation - simplest form of RFC5870
ppLocationReference :: LocationReference -> String
-- ppLocationReference (LocationCoord lat lng) = printf "%f,%f" lat lng -- new standard
ppLocationReference (LocationReferenceNum str) = str


instance Show LocationReference where
  show = ppLocationReference

instance ToSchema LocationReference

instance URI LocationEPC where
  printURI (SGLN companyPrefix (LocationReferenceNum str) (Just ext)) =
    "urn:epc:id:sgln:" ++ companyPrefix ++ "." ++ str ++ "." ++ ext
  printURI (SGLN companyPrefix (LocationReferenceNum str) Nothing) =
    "urn:epc:id:sgln:" ++ companyPrefix ++ "." ++ str

  readURI epcStr
   | isLocationEPC (splitOn ":" epcStr) =
      readURILocationEPC $ splitOn "." $ last $ splitOn ":" epcStr
   | otherwise            = Nothing

isLocationEPC :: [String] -> Bool
isLocationEPC ("urn" : "epc" : "id" : "sgln" : _) = True
isLocationEPC _                                   = False

-- returns Nothing if string cannot be parsed into lat and long
-- now deprecated
parseCoord :: [String] -> Maybe [String]
parseCoord ["latLong", lat, long] = Just [lat, long]
parseCoord _ = Nothing

-- checks if the string has coords. not a fully generalised function
-- now deprecated
hasCoord :: String -> Bool
hasCoord s = isJust obj
  where
    obj = parseCoord $ splitOn "-" s

-- GS1_EPC_TDS_i1_11.pdf Page 29
sglnPaddedComponentLength :: Int
sglnPaddedComponentLength = 12

readURILocationEPC :: [String] -> Maybe LocationEPC
-- without extension
readURILocationEPC [companyPrefix, locationStr]
  | length (companyPrefix ++ locationStr) /= sglnPaddedComponentLength = Nothing
  | otherwise = Just $ SGLN companyPrefix (LocationReferenceNum locationStr) Nothing
-- with extension
readURILocationEPC [companyPrefix, locationStr, ext]
  | length (companyPrefix ++ locationStr) /= sglnPaddedComponentLength = Nothing
  | otherwise = Just $ SGLN companyPrefix (LocationReferenceNum locationStr) (Just ext)
readURILocationEPC _ = Nothing -- error condition / invalid input

$(deriveJSON defaultOptions ''LocationReference)
$(deriveJSON defaultOptions ''LocationEPC)

instance ToSchema LocationEPC


-- |SourceDestType
data SourceDestType = SDOwningParty
                    | SDProcessingParty
                    | SDLocation
                    deriving (Show, Eq, Generic, Read)
$(deriveJSON defaultOptions ''SourceDestType)
instance ToSchema SourceDestType

instance URI SourceDestType where
  printURI = printSrcDestURI
  readURI epc = readSrcDestURI $ last $ splitOn ":" epc --FIXME - fixed @SA

srcDestPrefixStr :: String
srcDestPrefixStr = "urn:epcglobal:cbv:sdt:"

printSrcDestURI :: SourceDestType -> String
printSrcDestURI SDOwningParty = srcDestPrefixStr ++ "owning_party"
printSrcDestURI SDProcessingParty = srcDestPrefixStr ++ "processing_party"
printSrcDestURI SDLocation = srcDestPrefixStr ++ "location"

readSrcDestURI :: String -> Maybe SourceDestType
readSrcDestURI "owning_party" = Just SDOwningParty
readSrcDestURI "processing_party" = Just SDProcessingParty
readSrcDestURI "location" = Just SDLocation
readSrcDestURI _ = Nothing
{-
mkSourceDestType :: String -> Maybe SourceDestType
mkSourceDestType = mkByName

parseSourceDestType :: String -> Maybe SourceDestType
parseSourceDestType s = let uri = "urn:epcglobal:cbv:sdt" in
                            parseURI s uri :: Maybe SourceDestType

-}
-- https://github.csiro.au/Blockchain/GS1Combinators/blob/master/doc/GS1_EPC_TDS_i1_11.pdf
type DocumentType = String
type ServiceReference = String
data BusinessTransactionEPC =  GDTI GS1CompanyPrefix DocumentType SerialNumber
                             | GSRN GS1CompanyPrefix SerialReference
                              deriving (Show, Read, Eq, Generic)

-- urn:epc:id:gdti:CompanyPrefix.DocumentType.SerialNumber
instance URI BusinessTransactionEPC where
  printURI = printURIBusinessTransactionEPC
  readURI epcStr = readURIBusinessTransactionEPC $
                      getSuffixTokens [last $ splitOn ":" epcStr]
--                    Getting the uri body out of the string
printURIBusinessTransactionEPC :: BusinessTransactionEPC -> String
printURIBusinessTransactionEPC (GDTI gs1CompanyPrefix documentType serialNumber) =
  "urn:epc:id:gsrn:" ++ intercalate "." [gs1CompanyPrefix, documentType, serialNumber]
printURIBusinessTransactionEPC (GSRN gs1CompanyPrefix serialReference) =
  "urn:epc:id:gsrn:" ++ intercalate "." [gs1CompanyPrefix, serialReference]

-- the length of the arguments should equal to the following, according to the spec
-- used for the purposes of validation

-- GS1_EPC_TDS_i1_11.pdf Page 31
gsrnPaddedComponentLength :: Int
gsrnPaddedComponentLength = 17

-- GS1_EPC_TDS_i1_11.pdf Page 32
gdtiPaddedComponentLength :: Int
gdtiPaddedComponentLength = 12

readURIBusinessTransactionEPC :: [String] -> Maybe BusinessTransactionEPC
readURIBusinessTransactionEPC [gs1CompanyPrefix, serialReference]
  | length (gs1CompanyPrefix ++ serialReference) == gsrnPaddedComponentLength
    = Just $ GSRN gs1CompanyPrefix serialReference
  | otherwise = Nothing
readURIBusinessTransactionEPC [gs1CompanyPrefix, documentType, serialNumber]
  | length (gs1CompanyPrefix ++ documentType ++ serialNumber) == gdtiPaddedComponentLength
    = Just $ GDTI documentType documentType serialNumber
  | otherwise = Nothing
readURIBusinessTransactionEPC _ = Nothing

$(deriveJSON defaultOptions ''BusinessTransactionEPC)
instance ToSchema BusinessTransactionEPC


-- |TODO TEMP EPCClass is a String
newtype EPCClass = EPCClass String
  deriving (Eq, Show, Generic)
$(deriveJSON defaultOptions ''EPCClass)
instance ToSchema EPCClass

-- |TODO more restrictions here in the future
mkEPCClass :: String -> Maybe EPCClass
mkEPCClass x = Just $ EPCClass x


-- |Allocated by the company to a specific location
type LocationRef = String


data LocationError
  = IllegalGLNFormat
  | InvalidChecksum
  deriving (Show, Eq, Generic)


---------------------------
-- WHAT -------------------
---------------------------

data BizStep = Accepting
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

makeClassy ''BizStep
-- XXX - you might also want makeClassyPrisms for BizStep (as well as, or instead of, makeClassy)

-- DELETEME since not used, redundant
-- mkBizStep' :: String -> Maybe BizStep
-- mkBizStep' = mkByName

ppBizStep :: BizStep -> String
ppBizStep = revertCamelCase . show

bizstepPrefixStr = "urn:epcglobal:cbv:bizstep:"

instance URI BizStep where
  printURI epc = bizstepPrefixStr ++ ppBizStep epc
  readURI = mkBizStep

mkBizStep :: String -> Maybe BizStep
mkBizStep s  = let uri = "urn:epcglobal:cbv:bizstep" in
                      parseURI s uri :: Maybe BizStep

{-
  Example:

  <bizTransactionList>
    <bizTransaction type="urn:epcglobal:cbv:btt:po">
      http://transaction.acme.com/po/12345678
    </bizTransaction>
  </bizTransactionList>

-}


type BizTransactionID = String

data BizTransactionType = Bol       -- Bill of Lading
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

ppBizTransactionType :: BizTransactionType -> String
ppBizTransactionType = revertCamelCase . show

instance URI BizTransactionType where
  printURI   btt  = "urn:epcglobal:cbv:btt:" ++ show btt
  readURI         = parseBizTransactionType

-- DELETEME since redundant
-- mkBizTransactionType :: String -> Maybe BizTransactionType
-- mkBizTransactionType = mkByName

parseBizTransactionType :: String -> Maybe BizTransactionType
parseBizTransactionType s = let uri = "urn:epcglobal:cbv:btt" in
                                parseURI s uri :: Maybe BizTransactionType

-- |BizTransaction CBV Section 7.3 and Section 8.5
data BizTransaction = BizTransaction
  {
    _btid :: BizTransactionID
  , _bt   :: BizTransactionType
  }
  deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions ''BizTransaction)
instance ToSchema BizTransaction

makeClassy ''BizTransaction

-- | TransactionType, TransactionID
mkBizTransaction :: String -> String -> Maybe BizTransaction
mkBizTransaction t i = let bt' = parseBizTransactionType t in
                           case bt' of
                             Just t'  -> Just BizTransaction{_btid = i, _bt = t'}
                             _       -> Nothing


-- | TransformationID
type TransformationID = String

-- | ParentID
type ParentID = LabelEPC

data Action = Add
            | Observe
            | Delete
            deriving (Show, Eq, Generic, Read)
$(deriveJSON defaultOptions ''Action)
instance ToSchema Action

mkAction :: String -> Maybe Action
mkAction s = mkByName . camelCase $ toLower <$> s





---------------------------
-- WHY  -------------------
---------------------------

data DispositionError = InvalidDisposition
                      | OtherDispositionError
                      deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions ''DispositionError)
instance ToSchema DispositionError

makeClassyPrisms ''DispositionError

data Disposition = Active
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

makeClassyPrisms ''Disposition

ppDisposition :: Disposition -> String
ppDisposition = revertCamelCase . show

-- DELETEME since redundant, not used
-- mkDisposition' :: String -> Maybe Disposition
-- mkDisposition' = mkByName

instance URI Disposition where
  printURI disp =  "urn:epcglobal:cbv:disp:" ++ ppDisposition disp
  readURI       = mkDisposition

mkDisposition :: String -> Maybe Disposition
mkDisposition s = let uri = "urn:epcglobal:cbv:disp" in
                         parseURI s uri :: Maybe Disposition


---------------------------
-- WHEN  -------------------
---------------------------
{-
   A timestamp, giving the date and time in a time zone-independent manner.
   For bindings in which fields of this type are represented textually,
   an ISO-8601 compliant representation SHOULD be used.
-}
-- |The TimeZone will be saved independently
type EPCISTime = UTCTime

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

makeClassyPrisms ''EPCISTimeError

-- example format: 2005-04-03T20:33:31.116-06:00
-- |parse the string to UTC time, the time zone information will be merged into the time
parseStr2Time :: (AsEPCISTimeError e, MonadError e m) => String -> m EPCISTime
parseStr2Time s = let parsed = parseTimeM True defaultTimeLocale "%FT%X%Q%z" s :: Maybe EPCISTime in
                      case parsed of
                        Just et -> pure et
                        Nothing -> throwing _IllegalTimeFormat ()

-- |parse the string and obtain TimeZone,
parseStr2TimeZone :: (AsEPCISTimeError e, MonadError e m) => String -> m TimeZone
parseStr2TimeZone s = let parsed = parseTimeM True defaultTimeLocale "%FT%X%Q%z" s :: Maybe ZonedTime in
                      case parsed of
                        Just t -> let tz = zonedTimeZone t :: TimeZone in
                                      pure tz
                        Nothing -> throwing _IllegalTimeFormat ()




instance Eq ZonedTime where
  x == y = show x == show y

$(deriveJSON defaultOptions ''TimeZone)
--instance ToSchema ZonedTime
instance ToParamSchema TimeZone where
  toParamSchema _ = mempty
    & type_ .~ SwaggerString

instance ToField TimeZone where
  toField = toField . pack . show

-- copied from
-- https://hackage.haskell.org/package/swagger2-2.1.3/docs/src/Data.Swagger.Internal.Schema.html#line-477
named :: T.Text -> Schema -> NamedSchema
named name = NamedSchema (Just name) -- this function has been Eta reduced

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
