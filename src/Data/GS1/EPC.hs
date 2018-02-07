{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Data.GS1.EPC where

import           Control.Lens
import           GHC.Generics
import qualified Data.Text as T
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Swagger

import           Data.Time
import           Data.ByteString.Char8 (pack)
import           Data.GS1.Utils
import           Data.UUID (UUID)
import           Database.SQLite.Simple.ToField

-- add more type values to this if need be
data ParseFailure = InvalidLength
                  -- Length is not correct
                  -- CHECK in Disposition, InvalidFormat can also indicate wrong payload... FIXME?
                  | InvalidFormat
                  -- Components Missing, incorrectly structured
                  | InvalidAction
                  -- When parsing an action failed
                  | InvalidBizTransaction
                  -- When parsing a bizTransaction failed
                  | InvalidEvent
                  -- When parsing an event failed
                  | TimeZoneError
                  -- error in parsing timezone
                  | TagNotFound
                  -- when a mandatory tag is not found
                  | InvalidDispBizCombination
                  -- when the disposition does not go with the bizstep
                  | ChildFailure 
                    { 
                      _childFailuresList :: [ParseFailure]
                    }
                  -- when there is a list of Parsefailures
                  -- typically applicable to higher level structures,
                  -- like DWhat, DWhere, etc
                  deriving (Show, Eq)

-- |Anything that could be converted into URI
class URI a where
  printURI      :: a -> T.Text
  readURI       :: URI a => T.Text -> Either ParseFailure a

-- |Assigned by a GS1 Member Organisation to a user/subscriber
type GS1CompanyPrefix = T.Text
type ItemReference = T.Text
type ExtensionDigit = Int
type SerialReference = T.Text
-- |Calculated according to an algorithm https://en.wikipedia.org/wiki/Global_Location_Number
type CheckDigit = Int
type Lot = T.Text
type IndividualAssetReference = T.Text
type SerialNumber = T.Text
type SGLNExtension = T.Text

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

type Uom = T.Text
type Amount = Float
type AssetType = T.Text

data Quantity = MeasuredQuantity
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
data ClassLabelEPC =  LGTIN
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

instance ToField ClassLabelEPC where
  toField = toField . pack . show

-- move GRAI to InstanceLabel
-- implement reader for :idpat:sgtin:
readURIClassLabelEPC :: [T.Text] -> Either ParseFailure ClassLabelEPC
readURIClassLabelEPC ("urn" : "epc" : "class" : "lgtin" : rest) =
  Right $ LGTIN gs1CompanyPrefix itemReference lot
    where [gs1CompanyPrefix, itemReference, lot] = getSuffixTokens rest
readURIClassLabelEPC ("urn" : "epc" : "idpat" : "sgtin" : rest) =
  Right $ CSGTIN gs1CompanyPrefix Nothing itemReference
    where (gs1CompanyPrefix:itemReference:_) = getSuffixTokens rest
readURIClassLabelEPC _ = Left InvalidFormat

printURIClassLabelEPC :: ClassLabelEPC -> T.Text
printURIClassLabelEPC (LGTIN gs1CompanyPrefix itemReference lot) =
  T.append "urn:epc:class:lgtin:"
      (T.intercalate "." [gs1CompanyPrefix, itemReference, lot])

printURIClassLabelEPC (CSGTIN gs1CompanyPrefix _ itemReference) =
  T.append "urn:epc:idpat:sgtin:"
      (T.intercalate "." [gs1CompanyPrefix, itemReference])

$(deriveJSON defaultOptions ''ClassLabelEPC)
instance ToSchema ClassLabelEPC


data InstanceLabelEPC = GIAI 
                        {
                          _giaiCompanyPrefix :: GS1CompanyPrefix
                        , _giaiSerialNum     :: SerialNumber
                        }
                      -- Global Individual Asset Identifier,
                      -- e.g. bucket for olives
                      | SSCC
                        {
                          _ssccCompanyPrefix :: GS1CompanyPrefix
                        , _ssccSerialNum     :: SerialNumber
                        }
                      --serial shipping container code
                      | SGTIN
                        {
                          _sgtinCompanyPrefix    :: GS1CompanyPrefix
                        , _sgtinSgtinFilterValue :: Maybe SGTINFilterValue
                        , _sgtinItemReference    :: ItemReference
                        , _sgtinSerialNum        :: SerialNumber
                        }
                       --serialsed global trade item number
                      | GRAI
                        {
                          _graiCompanyPrefix :: GS1CompanyPrefix
                        , _graiAssetType     :: AssetType
                        , _graiSerialNum     :: SerialNumber
                        }
                     --Global returnable asset identifier
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

readURIInstanceLabelEPC :: [T.Text] -> Either ParseFailure InstanceLabelEPC

readURIInstanceLabelEPC ("urn" : "epc" : "id" : "giai" : rest) =
  Right $ GIAI gs1CompanyPrefix individualAssetReference
    where [gs1CompanyPrefix, individualAssetReference] = getSuffixTokens rest

readURIInstanceLabelEPC ("urn" : "epc" : "id" : "sscc" : rest)
  | isCorrectLen = Right $ SSCC gs1CompanyPrefix serialNumber
  | otherwise = Left InvalidLength
      where
        [gs1CompanyPrefix, serialNumber] = getSuffixTokens rest
        isCorrectLen =
            getTotalLength [gs1CompanyPrefix, serialNumber] == ssccPadLen

readURIInstanceLabelEPC ("urn" : "epc" : "id" : "grai" : rest) =
  Right $ GRAI gs1CompanyPrefix assetType serialNumber
    where [gs1CompanyPrefix, assetType, serialNumber] = getSuffixTokens rest

readURIInstanceLabelEPC ("urn" : "epc" : "id" : "sgtin" : rest)
  | isCorrectLen =
      Right $ SGTIN gs1CompanyPrefix Nothing itemReference serialNumber
                                  -- Nothing, for the moment
  | otherwise = Left InvalidLength
      where
        [gs1CompanyPrefix, itemReference, serialNumber] = getSuffixTokens rest
        isCorrectLen =
            getTotalLength [gs1CompanyPrefix, itemReference] == sgtinPadLen

readURIInstanceLabelEPC _ = Left InvalidFormat


printURIInstanceLabelEPC :: InstanceLabelEPC -> T.Text
printURIInstanceLabelEPC (GIAI gs1CompanyPrefix individualAssetReference) =
  T.append "urn:epc:id:giai:"
    (T.intercalate "." [gs1CompanyPrefix, individualAssetReference])
printURIInstanceLabelEPC (SSCC gs1CompanyPrefix serialNumber) =
  T.append "urn:epc:id:sscc:"
    (T.intercalate "." [gs1CompanyPrefix, serialNumber])
printURIInstanceLabelEPC (SGTIN gs1CompanyPrefix _ itemReference serialNumber) =
  T.append "urn:epc:id:sgtin:"
    (T.intercalate "." [gs1CompanyPrefix, itemReference, serialNumber])
printURIInstanceLabelEPC (GRAI gs1CompanyPrefix assetType serialNumber) =
  T.append "urn:epc:id:grai:"
    (T.intercalate "." [gs1CompanyPrefix, assetType, serialNumber])

$(deriveJSON defaultOptions ''InstanceLabelEPC)
instance ToSchema InstanceLabelEPC

instance ToField InstanceLabelEPC where
    toField = toField . pack . show


type Lng = Float
type Lat = Float
data LocationReference = LocationReferenceNum
                         {
                           _locationRefNum :: T.Text
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
-- ppLocationReference (LocationReferenceNum str) = str

instance ToSchema LocationReference

instance URI LocationEPC where
  printURI (SGLN companyPrefix (LocationReferenceNum str) (Just ext)) =
    T.append "urn:epc:id:sgln:"
      (T.intercalate "." [companyPrefix, str, ext])
  printURI (SGLN companyPrefix (LocationReferenceNum str) Nothing) =
    T.append "urn:epc:id:sgln:"
      (T.intercalate "." [companyPrefix, str])

  readURI epcStr
   | isLocationEPC (T.splitOn ":" epcStr) =
      readURILocationEPC $ T.splitOn "." $ last $ T.splitOn ":" epcStr
   | otherwise            = Left InvalidFormat

isLocationEPC :: [T.Text] -> Bool
isLocationEPC ("urn" : "epc" : "id" : "sgln" : _) = True
isLocationEPC _                                   = False

-- GS1_EPC_TDS_i1_11.pdf Page 29
sglnPadLen :: Int
sglnPadLen = 12

getExt :: T.Text -> Maybe SGLNExtension
getExt "0" = Nothing
getExt s   = Just s

readURILocationEPC :: [T.Text] -> Either ParseFailure LocationEPC
-- without extension
readURILocationEPC [companyPrefix, locationStr]
  | isCorrectLen =
      Right $ SGLN companyPrefix (LocationReferenceNum locationStr) Nothing
  | otherwise    = Left InvalidLength
    where
      isCorrectLen = getTotalLength [companyPrefix, locationStr] == sglnPadLen

-- with extension
readURILocationEPC [companyPrefix, locationStr, extNum]
  | isCorrectLen =
      Right $
        SGLN companyPrefix (LocationReferenceNum locationStr) (getExt extNum)
  | otherwise    = Left InvalidLength
    where
      isCorrectLen = getTotalLength [companyPrefix, locationStr] == sglnPadLen

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
readSrcDestURI "owning_party" = Right SDOwningParty
readSrcDestURI "possessing_party" = Right SDPossessingParty
readSrcDestURI "location" = Right SDLocation
readSrcDestURI _ = Left InvalidFormat

-- https://github.csiro.au/Blockchain/GS1Combinators/blob/master/doc/GS1_EPC_TDS_i1_11.pdf
type DocumentType = T.Text
type ServiceReference = T.Text
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
printURIBizTransactionEPC (GDTI gs1CompanyPrefix documentType serialNumber) =
  T.append "urn:epc:id:gsrn:"
    (T.intercalate "." [gs1CompanyPrefix, documentType, serialNumber])
printURIBizTransactionEPC (GSRN gs1CompanyPrefix serialReference) =
  T.append "urn:epc:id:gsrn:"
    (T.intercalate "." [gs1CompanyPrefix, serialReference])

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
readURIBusinessTransactionEPC [gs1CompanyPrefix, serialReference]
  | isCorrectLen = Right $ GSRN gs1CompanyPrefix serialReference
  | otherwise = Left InvalidLength
  where
    isCorrectLen =
        getTotalLength [gs1CompanyPrefix, serialReference] == gsrnPadLen
readURIBusinessTransactionEPC [gs1CompanyPrefix, documentType, serialNumber]
  | isCorrectLen = Right $ GDTI documentType documentType serialNumber
  | otherwise = Left InvalidLength
  where
    isCorrectLen =
        getTotalLength [gs1CompanyPrefix, documentType, serialNumber] ==
          gdtiPadLen
readURIBusinessTransactionEPC _ = Left InvalidFormat

$(deriveJSON defaultOptions ''BusinessTransactionEPC)
instance ToSchema BusinessTransactionEPC


-- |Allocated by the company to a specific location
type LocationRef = T.Text


data LocationError
  = IllegalGLNFormat
  | InvalidChecksum
  deriving (Show, Eq, Generic)


---------------------------
-- WHAT -------------------
---------------------------

-- CBV-Standard-1-2-r-2016-09-29.pdf Page 17
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

ppBizStep :: BizStep -> T.Text
ppBizStep = revertCamelCase . T.pack . show

bizstepPrefixStr :: T.Text
bizstepPrefixStr = "urn:epcglobal:cbv:bizstep:"

readURIBizStep :: Maybe BizStep -> Either ParseFailure BizStep
readURIBizStep Nothing = Left InvalidFormat
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


type BizTransactionID = T.Text

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

ppBizTransactionType :: BizTransactionType -> T.Text
ppBizTransactionType = revertCamelCase . T.pack . show

readURIBizTransactionType :: Maybe BizTransactionType ->
                              Either ParseFailure BizTransactionType
readURIBizTransactionType Nothing = Left InvalidFormat
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
type TransformationID = UUID

data Action = Add
            | Observe
            | Delete
            deriving (Show, Eq, Generic, Read)
$(deriveJSON defaultOptions ''Action)
instance ToSchema Action

mkAction :: T.Text -> Either ParseFailure Action
mkAction t =
  case mkByName . camelCase $ T.toLower t of
    Nothing -> Left InvalidAction
    Just x  -> Right x

---------------------------
-- WHY  -------------------
---------------------------

data DispositionError = InvalidDisposition
                      | OtherDispositionError
                      deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions ''DispositionError)
instance ToSchema DispositionError


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


ppDisposition :: Disposition -> T.Text
ppDisposition = revertCamelCase . T.pack . show

-- CBV-Standard-1-2-r-2016-09-29.pdf page 24
readURIDisposition :: Maybe Disposition -> Either ParseFailure Disposition
readURIDisposition Nothing = Left InvalidFormat
readURIDisposition (Just disp) = Right disp

instance URI Disposition where
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
