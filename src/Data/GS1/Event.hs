{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.GS1.Event where

import           Control.Lens.TH
import           Control.Monad.Error.Lens
import           Control.Monad.Except     (MonadError)
import           Data.GS1.Location
import           Data.GS1.Object
import           Data.GS1.URI
import           Data.GS1.Utils
import           Data.List
import           Data.Maybe
import           Data.Time.Clock
import           Data.Time.LocalTime
import           GHC.Generics


data BizStepError = InvalidDisposition
                  | OtherBizStepError
                  deriving (Show, Eq, Generic)

makeClassyPrisms ''BizStepError

{-
   A timestamp, giving the date and time in a time zone-independent manner.
   For bindings in which fields of this type are represented textually,
   an ISO-8601 compliant representation SHOULD be used.
-}
type EPCISTime = UniversalTime

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
    deriving (Show,Eq,Generic)

ppBizStep :: BizStep -> String
ppBizStep = revertCamelCase . show

instance URI BizStep where
  uriPrefix _        = "urn:epcglobal:cbv"
  uriQuantifier _    = "bizstep"
  uriPayload bizStep = ppBizStep bizStep

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
    deriving (Show,Eq,Generic)

ppDisposition :: Disposition -> String
ppDisposition = revertCamelCase . show

instance URI Disposition where
  uriPrefix _     = "urn:epcglobal:cbv"
  uriQuantifier _ = "disp"
  uriPayload      = ppDisposition

-- Valid Dispositions, defined in section CBV 7.2
dispositionValidList :: Disposition -> [BizStep]
dispositionValidList Active                =  [Commissioning]
dispositionValidList ContainerClosed       =  [StagingOutbound]
dispositionValidList Damaged               =  [Accepting, Inspecting, Receiving, Removing, Repairing, Replacing]
dispositionValidList Destroyed             =  [Destroying]
dispositionValidList Dispensed             =  [] -- nothing defined - page 25 of spec
dispositionValidList Encoded               =  [Encoding]
dispositionValidList Expired               =  [Holding, StagingOutbound, Storing]
dispositionValidList InProgress            =  [Receiving, Picking, Loading, Accepting, StagingOutbound, Arriving, VoidShipping]
dispositionValidList InTransit             =  [Shipping, Departing]
dispositionValidList Inactive              =  [Decommissioning]
dispositionValidList NoPedigreeMatch       =  [Holding, StagingOutbound, Storing]
dispositionValidList NonSellableOther      =  [Holding, Inspecting, StagingOutbound, Storing]
dispositionValidList PartiallyDispensed    =  []  -- nothing defined - page 25 of spec
dispositionValidList Recalled              =  [Holding, StagingOutbound, Storing]
dispositionValidList Reserved              =  [Reserving]
dispositionValidList RetailSold            =  [RetailSelling]
dispositionValidList Returned              =  [Receiving, Holding, Shipping]
dispositionValidList SellableAccessible    =  [Stocking, Receiving]
dispositionValidList SellableNotAccessible =  [Receiving, Storing, Loading, Holding, Inspecting]
dispositionValidList Stolen                =  [] -- nothing defined - page 25 of spec
dispositionValidList Unknown               =  [] -- nothing defined - page 25 of spec

dispositionValidFor :: BizStep -> Disposition -> Bool
dispositionValidFor b d = b `elem` dispositionValidList d

{-
type BizTransactionIdentifier = Maybe String --FIXME - user defined element
data BizTransactionType = Bol BizTransactionIdentifier
                             | Desadv BizTransactionIdentifier
                             | Inv BizTransactionIdentifier
                             | Pedigree BizTransactionIdentifier
                             | Po BizTransactionIdentifier
                             | Poc BizTransactionIdentifier
                             | Prodorder BizTransactionIdentifier
                             | Recadv BizTransactionIdentifier
                             | Rma BizTransactionIdentifier
    deriving (Show,Eq,Generic)
-}

-- |BizTransaction CBV Section 7.3 and Section 8.5
-- BTI stands for Business Transaction Identifier
-- BTT stands for Business Transaction Type
-- GDTI stands for Global Document Type Identifier
-- GSRN stands for Global Service Relation Number
-- MAY contain one or more BizTransactionType means [0..*]
data BizTransaction = BizTransactionID [BizTransactionType]

data BizTransactionID = BTIGDTI String
                      | BTIGSRN String
                      | BTIGLN String

instance URI BizTransactionID where
  uriPrefix a = case a of
                  BTIGDTI _ -> "urn:epc:id"
                  BTIGSRN _ -> "urn:epc:id"
                  BTIGLN _  -> "urn:epcglobal:cbv:bt"
  uriQuantifier a = case a of
                      BTIGDTI _ -> "gdti"
                      BTIGSRN _ -> "gsrn"
                      BTIGLN  _ -> "gln"
  uriPayload a = case a of
                   BTIGDTI b -> b
                   BTIGSRN b -> b
                   BTIGLN b  -> b

data BizTransactionType = Bol       -- Bill of Lading
                        | Desadv    -- Dispatch Advice
                        | Inv       -- Invoice
                        | Pedigree  -- Pedigree
                        | Po        -- Purchase Order
                        | Poc       -- Purchase Order Confirmation
                        | Prodorder -- Production Order
                        | Recadv    -- Receiving Advice
                        | Rma       -- Return Mechandise Authorisation
                        deriving (Show, Eq, Generic)

ppBizTransactionType :: BizTransactionType -> String
ppBizTransactionType = revertCamelCase . show

instance URI BizTransactionType where
  uriPrefix _     = "urn:epcglobal:cbv"
  uriQuantifier _ = "btt"
  uriPayload      = ppBizTransactionType

-- EPCIS 1.2 section 7.3.5.4 line 1150
-- Example can be found at EPCIS 1.2 section 9.6.2 line [3319..3340]
data SourceDestID = SourceDestID String
  deriving (Show, Eq, Generic)

instance URI SourceDestID where
  uriPrefix _                 = "urn:epc:id"
  uriQuantifier _             = "sgln"
  uriPayload (SourceDestID s) = s

data SourceDestType = SDOwningParty
                    | SDProcessingParty
                    | SDLocation
  deriving (Show, Eq, Generic)

instance URI SourceDestType where
  uriPrefix _     = "urn:epcglobal:cbv"
  uriQuantifier _ = "sdt"
  uriPayload a    = case a of
                      SDOwningParty     -> "owning_party"
                      SDProcessingParty -> "processing_party"
                      SDLocation        -> "location"

data Source = Source SourceDestType SourceDestID
  deriving (Show, Eq, Generic)

data Destination = Destination SourceDestType SourceDestID
  deriving (Show, Eq, Generic)

-- |EPCIS 1.2 section 7.5
-- FIXME example should be found to verify the implementation is correct
data ErrorReasonID = DidNotOccur
                   | IncorrectData
                   deriving (Show, Eq, Generic)

ppErrorReasonID :: ErrorReasonID -> String
ppErrorReasonID e = case e of
                      DidNotOccur   -> "did_not_occur"
                      IncorrectData -> "incorrect_data"

data ErrorDeclaration = ErrorDeclaration ErrorReasonID
  deriving (Show, Eq, Generic)

instance URI ErrorDeclaration where
  uriPrefix _                     = "urn:epcglobal:cbv"
  uriQuantifier _                 = "er"
  uriPayload (ErrorDeclaration r) = ppErrorReasonID r

--TODO
type TransformationID = String -- FIXME user defined element

data When = When {
  _eventTime  :: EPCISTime
, _recordTime :: EPCISTime
, _timeZone   :: TimeZone
} deriving (Show, Eq, Generic)


data Where = Where {
  _readPoint   :: (Maybe ReadPointLocation)
, _bizLocation :: (Maybe BizLocation)
, _srcDestType :: (Maybe [SourceDestType])
} deriving (Show, Eq, Generic)

data What = ObjectWhat {
              _objects :: [ObjectID],
              _action  :: Action,
              _btt     :: [BizTransactionType],
              _ilmd    :: Maybe Ilmd
            }
          | AggregationWhat {
              _parentID :: Maybe ObjectID,
              _objects  :: [ObjectID],
              _action   :: Action,
              _btt      :: [BizTransactionType]
            }
          | QuantityWhat {
              _objects :: [ObjectID],
              _btt     :: [BizTransactionType]
            }
          | TransformationWhat {
              _input            :: [ObjectID],
              _output           :: [ObjectID],
              _transformationID :: TransformationID,
              _btt              :: [BizTransactionType],
              _ilmd             :: Maybe Ilmd
            }
          | TransactionWhat {
              _parentID :: Maybe ObjectID,
              _objects  :: [ObjectID],
              _action   :: Action,
              _btt      :: [BizTransactionType]
            }
  deriving (Show, Eq, Generic)

data EventType = ObjectEvent | AggregationEvent | QuantityEvent |
  TransactionEvent | TransformationEvent  deriving (Show,Eq,Generic)

type EventID = Int --FIXME - user defined element

data Action = Add | Observe | Delete  deriving (Show,Eq,Generic)

data Event = Event {
  _type  :: EventType
, _id    :: EventID
, _what  :: What
, _when  :: When
, _why   :: Why
, _where :: Where
} deriving (Show, Eq, Generic)


objectEvent :: EventID -> [ObjectID] -> Action -> [BizTransactionType] ->
    Maybe Ilmd ->When -> Why -> Where -> Event
objectEvent id objects action btt ilmd when why whre =
  Event ObjectEvent id (ObjectWhat objects action btt ilmd) when why whre

--TODO: check parent is present when needed (based on action)
aggregationEvent :: EventID -> Maybe ObjectID -> [ObjectID] -> Action ->
  [BizTransactionType] -> When -> Why -> Where -> Event
aggregationEvent id parent objects action btt when why whre =
  Event AggregationEvent id (AggregationWhat parent objects action btt) when why whre

--TODO: check that all ObjectIDs are class objects with quantities.
quantityEvent :: EventID -> [ObjectID] -> [BizTransactionType] ->
  When -> Why -> Where -> Event
quantityEvent id objects btt when why whre=
  Event QuantityEvent id (QuantityWhat objects btt) when why whre

transformationEvent :: EventID -> [ObjectID] -> [ObjectID] -> TransformationID
                    -> [BizTransactionType] -> Maybe Ilmd -> When -> Why ->
                      Where -> Event
transformationEvent id inputs outputs transformID btt  ilmd when why whre =
  Event TransformationEvent id (TransformationWhat inputs outputs transformID btt ilmd)
    when why whre

transactionEvent :: EventID -> Maybe ObjectID -> [ObjectID] -> Action ->
  [BizTransactionType] -> When -> Why -> Where -> Event
transactionEvent id parentID objects action btt when why whre =
  Event TransactionEvent id (TransactionWhat parentID objects action btt) when why whre

data Why = Why {_bizStep :: Maybe BizStep , _disposition :: Maybe Disposition}
  deriving (Show, Eq, Generic)

-- TODO Why derive lens
--makeClassy ''Why

why :: (AsBizStepError e, MonadError e m)
     => Maybe BizStep -> Maybe Disposition -> m Why
why step disp
  | isNothing step || isNothing disp = pure (Why step disp)  -- TODO: verify when encounter Nothing
  | otherwise                        = if dispositionValidFor (fromJust step) (fromJust disp)
                                          then pure (Why step disp)
                                          else throwing _InvalidDisposition ()

data WhyCBVCompliant = WhyCBVCompliant BizStep (Maybe Disposition)
  deriving (Show, Eq, Generic)

-- TODO can we verify validity better when disposition is Nothing?
whyCBVCompliant :: (AsBizStepError e, MonadError e m)
     => BizStep -> Maybe Disposition -> m WhyCBVCompliant
whyCBVCompliant step disp = case disp of
                              Nothing -> pure (WhyCBVCompliant step Nothing)
                              Just d -> if dispositionValidFor step d
                                           then pure (WhyCBVCompliant step (Just d))
                                           else throwing _InvalidDisposition ()

{--
== Object Event ==
 An ObjectEvent Captures information about an event pertaining to one or more physical or digital objects identified by instance-level (EPC) or class-level (EPC Class) identifiers.
 While more than one EPC and/or EPC Class may appear in an ObjectEvent, no relationship or association between those objects i implied other than the coincidence of having experienced identical events in the real world
*denotes optional field
 - Fields -
 When - EventTime/RecordTime/EventTimeOffset
 What - EpcList* quantityList* (an ObjectEvent shall contain a non-empty epcList or a non-empty quantityList or both)
        Action
        Ilmd
        [BizTransactionType]*
 Why -
        BizStep*
        Disposition*

 Where -
        ReadPoint*
        BizLocation*
        [SrcDestType]*
--}

{--
== Aggregation Event ==

 - Fields -
 When - EventTime/RecordTime/EventTimeOffset
 What -
        ParentID** (Optional when Action == Observe, required otherwise)
        [ChildEPC]* (An AggregationEvent SHALL contain either a non-empty [childEPCs] a non-empty [childQuantity], or both, except that both may be empty if Action==Delete)
        [ChildQuantity]*
        Action
        [BizTransactionType]
 Why -
        BizStep*
        Disposition*
 Where -
        ReadPoint*
        BizLocation*
        [SrcDestType]
--}

{--
== Quantity Event ==
 - Fields -
 When - EventTime/RecordTime/EventTimeOffset
 What -
        EPCClass, Quantity
        [BizTransactionType]
 Why -
        BizStep*
        Disposition*
 Where -
        ReadPoint*
        BizLocation*
--}

{--
== Transaction Events ==
 - Fields -
 When - EventTime/RecordTime/EventTimeOffset
 What -
        [BizTransactionType]
        ParentID*
        EPCList, Quantity List
        Action
 Why -
        BizStep
        Disposition

 Where -
        ReadPoint
        BizLocation
        [SrcDestType]
--}

{--
== Transformation Events ==
 - Fields -
 When - EventTime/RecordTime/EventTimeOffset
 What - [InputEPC]*, [OutputQuantity]*
        [OutputEPC]*, [OutputQuantity]*
        TransformationID*
        [BizTransactionType]
        Ilmd*
 Why  -
        BizStep*
        Disposition*
 Where -
        ReadPoint*
        BizLocation*
        [SrcDestType]*
--}

