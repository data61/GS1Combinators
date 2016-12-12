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
ppBizStep bizStep = case bizStep of
                           Accepting             -> "accepting"
                           Arriving              -> "arriving"
                           Assembling            -> "assembling"
                           Collecting            -> "collecting"
                           Commissioning         -> "commissioning"
                           Consigning            -> "consigning"
                           CreatingClassInstance -> "creating_class_instance"
                           CycleCounting         -> "cycle_counting"
                           Decommissioning       -> "decommissioning"
                           Departing             -> "departing"
                           Destroying            -> "destroying"
                           Disassembling         -> "disassembling"
                           Dispensing            -> "dispensing"
                           Encoding              -> "encoding"
                           EnteringExiting       -> "entering_exiting"
                           Holding               -> "holding"
                           Inspecting            -> "inspecting"
                           Installing            -> "installing"
                           Killing               -> "killing"
                           Loading               -> "loading"
                           Other                 -> "other"
                           Packing               -> "packing"
                           Picking               -> "picking"
                           Receiving             -> "receiving"
                           Removing              -> "removing"
                           Repackaging           -> "repackaging"
                           Repairing             -> "repairing"
                           Replacing             -> "replacing"
                           Reserving             -> "reserving"
                           RetailSelling         -> "retail_selling"
                           Shipping              -> "shipping"
                           StagingOutbound       -> "staging_outbound"
                           StockTaking           -> "stock_taking"
                           Stocking              -> "stocking"
                           Storing               -> "storing"
                           Transporting          -> "transporting"
                           Unloading             -> "unloading"
                           VoidShipping          -> "void_shipping"

instance URI BizStep where
  ppURI bizStep      = intercalate ":" ["urn:epcglobal:cbv", "bizstep", ppBizStep bizStep]
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
ppDisposition disp = case disp of
                       Active                -> "active"
                       ContainerClosed       -> "container_closed"
                       Damaged               -> "damaged"
                       Destroyed             -> "destroyed"
                       Dispensed             -> "dispensed"
                       Disposed              -> "disposed"
                       Encoded               -> "encoded"
                       Expired               -> "expired"
                       InProgress            -> "in_progress"
                       InTransit             -> "in_transit"
                       Inactive              -> "inactive"
                       NoPedigreeMatch       -> "no_pedigree_match"
                       NonSellableOther      -> "non_sellable_other"
                       PartiallyDispensed    -> "partially_dispensed"
                       Recalled              -> "recalled"
                       Reserved              -> "reserved"
                       RetailSold            -> "retail_sold"
                       Returned              -> "returned"
                       SellableAccessible    -> "sellable_accessible"
                       SellableNotAccessible -> "sellable_not_accessible"
                       Stolen                -> "stolen"
                       Unknown               -> "unknown"

instance URI Disposition where
  ppURI disp      = intercalate ":" ["urn:epcglobal:cbv", "disp", ppDisposition disp]
  uriPrefix _     = "urn:epcglobal:cbv"
  uriQuantifier _ = "disp"
  uriPayload disp = ppDisposition disp

-- Valid Dispositions, defined in section CBV 7.2
dispositionValidList :: Disposition -> [BizStep]
dispositionValidList Active           =  [Commissioning]
dispositionValidList ContainerClosed =  [StagingOutbound]
dispositionValidList Damaged          =  [Accepting, Inspecting, Receiving, Removing, Repairing, Replacing]
dispositionValidList Destroyed  =  [Destroying]
dispositionValidList Dispensed  =  [] -- nothing defined - page 25 of spec
dispositionValidList Encoded    =  [Encoding]
dispositionValidList Expired    =  [Holding, StagingOutbound, Storing]
dispositionValidList InProgress=  [Receiving, Picking, Loading, Accepting, StagingOutbound, Arriving, VoidShipping]
dispositionValidList InTransit =  [Shipping, Departing]
dispositionValidList Inactive   =  [Decommissioning]
dispositionValidList NoPedigreeMatch   =  [Holding, StagingOutbound, Storing]
dispositionValidList NonSellableOther =  [Holding, Inspecting, StagingOutbound, Storing]
dispositionValidList PartiallyDispensed=  []  -- nothing defined - page 25 of spec
dispositionValidList Recalled   =  [Holding, StagingOutbound, Storing]
dispositionValidList Reserved   =  [Reserving]
dispositionValidList RetailSold=  [RetailSelling]
dispositionValidList Returned   =  [Receiving, Holding, Shipping]
dispositionValidList SellableAccessible     =  [Stocking, Receiving]
dispositionValidList SellableNotAccessible =  [Receiving, Storing, Loading, Holding, Inspecting]
dispositionValidList Stolen =  [] -- nothing defined - page 25 of spec
dispositionValidList Unknown =  [] -- nothing defined - page 25 of spec

dispositionValidFor :: BizStep -> Disposition -> Bool
dispositionValidFor bs disp = bs `elem` dispositionValidList disp

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
-- MAY contain one or more BizTransactionType means [0..*]
data BizTransaction = BizTransactionID [BizTransactionType]

data BizTransactionID = BTIGDTI String
                      | BTIGSRN String
                      | BTIGLN String

instance URI BizTransactionID where
  ppURI a = intercalate ":" [uriPrefix a, uriQuantifier a, uriPayload a]
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
ppBizTransactionType _btt = case _btt of
                             Bol       -> "bol"
                             Desadv    -> "desadv"
                             Inv       -> "inv"
                             Pedigree  -> "pedigree"
                             Po        -> "po"
                             Poc       -> "poc"
                             Prodorder -> "prodorder"
                             Recadv    -> "recadv"
                             Rma       -> "rma"

instance URI BizTransactionType where
  ppURI a         = intercalate ":" ["urn:epcglobal:cbv", "btt", ppBizTransactionType a]
  uriPrefix _     = "urn:epcglobal:cbv"
  uriQuantifier _ = "btt"
  uriPayload a    = ppBizTransactionType a

data SourceDestType = SourceDestType

data Source = Source

type TransformationID = String -- FIXME user defined element
type SrcDestID = String -- FIXME user defined element
data SrcDestType = OwningParty SrcDestID | PossessingParty SrcDestID
  deriving (Show,Eq,Generic)


data When = When {
    eventTime  :: EPCISTime,
    recordTime :: EPCISTime,
    timeZone   :: TimeZone
                 } deriving (Show,Eq,Generic)


data Where = Where {
  readPoint   :: (Maybe ReadPointLocation),
  bizLocation :: (Maybe BizLocation),
  srcDestType :: (Maybe [SrcDestType])
} deriving (Show,Eq,Generic)

data What = ObjectWhat {
              objects :: [ObjectID],
              action  :: Action,
              btt     :: [BizTransactionType],
              ilmd    :: Maybe Ilmd
            }
          | AggregationWhat {
              parentID :: Maybe ObjectID,
              objects  :: [ObjectID],
              action   :: Action,
              btt      :: [BizTransactionType]
            }
          | QuantityWhat {
              objects :: [ObjectID],
              btt     :: [BizTransactionType]
            }
          | TransformationWhat {
              input            :: [ObjectID],
              output           :: [ObjectID],
              transformationID :: TransformationID,
              btt              :: [BizTransactionType],
              ilmd             :: Maybe Ilmd
             }
          | TransactionWhat {
              parentID :: Maybe ObjectID,
              objects  :: [ObjectID],
              action   :: Action,
              btt      :: [BizTransactionType]
            } deriving (Show,Eq,Generic)


data EventType = ObjectEvent | AggregationEvent | QuantityEvent |
  TransactionEvent | TransformationEvent  deriving (Show,Eq,Generic)

type EventID = Int --FIXME - user defined element

data Action = Add | Observe | Delete  deriving (Show,Eq,Generic)

data Event = Event {
  _type  :: EventType,
  _id    :: EventID,
  _what  :: What,
  _when  :: When,
  _why   :: Why,
  _where :: Where
} deriving (Show,Eq,Generic)


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

data Why = Why (Maybe BizStep) (Maybe Disposition)
  deriving (Show, Eq, Generic)

why :: (AsBizStepError e, MonadError e m)
     => Maybe BizStep -> Maybe Disposition -> m Why
why step disp
  | isNothing step || isNothing disp = pure (Why step disp)  -- TODO: verify when encounter Nothing
  | otherwise                        = let v = dispositionValidFor (fromJust step) (fromJust disp) in
                                           case v of
                                             True -> pure (Why step disp)
                                             _    -> throwing _InvalidDisposition ()

data WhyCBVCompliant = WhyCBVCompliant BizStep (Maybe Disposition)
  deriving (Show, Eq, Generic)

whyCBVCompliant :: (AsBizStepError e, MonadError e m)
     => BizStep -> Maybe Disposition -> m WhyCBVCompliant
whyCBVCompliant step disp = case disp of
                              -- TODO can we verify validity better when disposition is Nothing?
                              Nothing -> pure (WhyCBVCompliant step Nothing)
                              Just d  -> let v = dispositionValidFor step d in
                                           case v of
                                             True -> pure (WhyCBVCompliant step (Just d))
                                             _    -> throwing _InvalidDisposition ()

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

