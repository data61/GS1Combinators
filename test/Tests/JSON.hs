{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Tests.JSON where

import           Data.Aeson
import           Data.Aeson.Types    (Pair)
import           Data.Text           (Text)
import           Data.Time.Calendar  (Day (..))
import           Data.Time.Clock     (UTCTime (..))
import           Data.Time.LocalTime (TimeZone (..))
import           Test.Hspec

import           Data.GS1.DWhat
import           Data.GS1.DWhen      (DWhen (..))
import           Data.GS1.DWhere
import           Data.GS1.DWhy
import           Data.GS1.EPC
import           Data.GS1.Event


testJSON :: Spec
testJSON = do
  toFromJSONSpec "TimeZone" timeZoneJSON timeZone
  toFromJSONSpec "EPCISTime" epcisTimeJSON epcisTime
  toFromJSONSpec "SourceLocation" sourceLocationJSON sourceLocation
  toFromJSONSpec "DestinationLocation" destinationLocationJSON destinationLocation

  toFromJSONSpec "ObjectEvent-IL"
                 (eventJSON "ObjectEvent" objectWhatILProps)
                 (event ObjectEventT objectWhatIL)

  toFromJSONSpec "ObjectEvent-CL"
                 (eventJSON "ObjectEvent" objectWhatCLProps)
                 (event ObjectEventT objectWhatCL)


  toFromJSONSpec "AggregationEvent-IL"
                 (eventJSON "AggregationEvent" aggregationWhatILProps)
                 (event AggregationEventT aggregationWhatIL)

  toFromJSONSpec "AggregationEvent-CL"
                 (eventJSON "AggregationEvent" aggregationWhatCLProps)
                 (event AggregationEventT aggregationWhatCL)

  toFromJSONSpec "TransactionEvent-IL"
                 (eventJSON "TransactionEvent" transactionWhatILProps)
                 (event TransactionEventT transactionWhatIL)

  toFromJSONSpec "TransactionEvent-CL"
                 (eventJSON "TransactionEvent" transactionWhatCLProps)
                 (event TransactionEventT transactionWhatCL)

  toFromJSONSpec "TransformationEvent-ILIL"
                 (eventJSON "TransformationEvent" transformationWhatILILProps)
                 (event TransformationEventT transformationWhatILIL)

  toFromJSONSpec "TransformationEvent-ILCLILCL"
                 (eventJSON "TransformationEvent" transformationWhatILCLILCLProps)
                 (event TransformationEventT transformationWhatILCLILCL)

  toFromJSONSpec "AssociationEvent-Pallet-Sensor"
                 (eventJSON "AssociationEvent" associationWhatPalletSensorProps)
                 (event AssociationEventT associationWhatPalletSensor)


toFromJSONSpec :: (ToJSON a, FromJSON a, Show a, Eq a) => String -> Value -> a -> SpecWith ()
toFromJSONSpec s v x =
  describe s $ do
    parsing v $ \a -> a `shouldBe` x
    writing x $ \a -> a `shouldBe` v

parsing :: FromJSON a => Value -> (a -> Expectation) -> SpecWith ()
parsing x f = it "can be parsed from JSON" $
  case fromJSON x of
    Error e    -> expectationFailure $ "no parse: " <> e
    Success a' -> f a'

writing :: ToJSON a => a -> (Value -> Expectation) -> SpecWith ()
writing x f =  it "can be written as JSON" $
  f $ toJSON x


-- Test Values and expected JSON

event :: EventType -> DWhat -> Event
event t w = Event t Nothing w when why where'

eventJSON :: Text -> [Pair] -> Value
eventJSON t w = object $ [ "isA" .= String t
                         ] <> w
                           <> whenProps
                           <> whyProps
                           <> whereProps


objectWhatIL :: DWhat
objectWhatIL = ObjWhat $ ObjectDWhat Add [ IL instanceLabelEPC_1234 ]

objectWhatILProps :: [Pair]
objectWhatILProps = [ "action" .= String "ADD"
                    , "epcList" .= [ instanceLabelEPCJSON_1234 ]
                    ]

objectWhatCL :: DWhat
objectWhatCL = ObjWhat $ ObjectDWhat Delete [ CL classLabelEPC_321 (Just $ ItemCount 100) ]

objectWhatCLProps :: [Pair]
objectWhatCLProps = [ "action" .= String "DELETE"
                    , "quantityList" .= [ object [ "epcClass" .= classLabelEPCJSON_321
                                                 , "quantity" .= Number 100
                                                 ]
                                        ]
                    ]


aggregationWhatIL :: DWhat
aggregationWhatIL = AggWhat $ AggregationDWhat Delete (Just $ ParentLabel instanceLabelEPC_1234) [ IL instanceLabelEPC_5678 ]

aggregationWhatILProps :: [Pair]
aggregationWhatILProps = [ "action" .= String "DELETE"
                         , "parentID" .= instanceLabelEPCJSON_1234
                         , "childEPCs" .= [ instanceLabelEPCJSON_5678 ]
                         ]



aggregationWhatCL :: DWhat
aggregationWhatCL = AggWhat $ AggregationDWhat Delete (Just $ ParentLabel instanceLabelEPC_1234) [ CL classLabelEPC_654 (Just $ ItemCount 19) ]

aggregationWhatCLProps :: [Pair]
aggregationWhatCLProps = [ "action" .= String "DELETE"
                         , "parentID" .= instanceLabelEPCJSON_1234
                         , "childQuantityList" .= [ object [ "epcClass" .= classLabelEPCJSON_654
                                                           , "quantity" .= Number 19
                                                           ]
                                                  ]
                         ]


transactionWhatIL :: DWhat
transactionWhatIL = TransactWhat $ TransactionDWhat Add (Just $ ParentLabel instanceLabelEPC_5678) [bizTransaction] [ IL instanceLabelEPC_1234 ]

transactionWhatILProps :: [Pair]
transactionWhatILProps = [ "action" .= String "ADD"
                         , "parentID" .= instanceLabelEPCJSON_5678
                         , "bizTransactionList" .= [ bizTransactionJSON ]
                         , "epcList" .= [ instanceLabelEPCJSON_1234 ]
                         ]

transactionWhatCL :: DWhat
transactionWhatCL = TransactWhat $ TransactionDWhat Delete Nothing [ bizTransaction ] [ CL classLabelEPC_321 (Just $ ItemCount 100) ]

transactionWhatCLProps :: [Pair]
transactionWhatCLProps = [ "action" .= String "DELETE"
                         , "bizTransactionList" .= [ bizTransactionJSON ]
                         , "quantityList" .= [ object [ "epcClass" .= classLabelEPCJSON_321
                                                      , "quantity" .= Number 100
                                                      ]
                                             ]
                         ]


transformationWhatILIL :: DWhat
transformationWhatILIL = TransformWhat $ TransformationDWhat Nothing
                                                             [ InputEPC $ IL instanceLabelEPC_1234 ]
                                                             [ OutputEPC $ IL instanceLabelEPC_5678 ]

transformationWhatILILProps :: [Pair]
transformationWhatILILProps = [ "inputEPCList" .= [ instanceLabelEPCJSON_1234 ]
                              , "outputEPCList" .= [ instanceLabelEPCJSON_5678 ]
                              ]


transformationWhatILCLILCL :: DWhat
transformationWhatILCLILCL = TransformWhat $ TransformationDWhat Nothing
                                                                 [ InputEPC $ IL instanceLabelEPC_1234
                                                                 , InputEPC $ CL classLabelEPC_321 (Just $ ItemCount 100)
                                                                 ]
                                                                 [ OutputEPC $ IL instanceLabelEPC_5678
                                                                 , OutputEPC $ CL classLabelEPC_654 (Just $ ItemCount 10)
                                                                 ]

transformationWhatILCLILCLProps :: [Pair]
transformationWhatILCLILCLProps = [ "inputEPCList" .= [ instanceLabelEPCJSON_1234 ]
                                  , "inputQuantityList" .= [ object [ "epcClass" .= classLabelEPCJSON_321
                                                                    , "quantity" .= Number 100
                                                                    ]
                                                           ]
                                  , "outputEPCList" .= [ instanceLabelEPCJSON_5678 ]
                                  , "outputQuantityList" .= [ object [ "epcClass" .= classLabelEPCJSON_654
                                                                     , "quantity" .= Number 10
                                                                     ]
                                                            ]
                                  ]


associationWhatPalletSensor :: DWhat
associationWhatPalletSensor = AssociationWhat $ AssociationDWhat ( ParentLabel palletLabel_1234 )
                                                                 [ IL sensorLabel_1234 ]

associationWhatPalletSensorProps :: [Pair]
associationWhatPalletSensorProps = [ "parentID" .= palletLabelJSON_1234
                                   , "childEPCs" .= [ sensorLabelJSON_1234 ]
                                   ]


when :: DWhen
when = DWhen epcisTime (Just epcisTime) timeZone

whenProps :: [Pair]
whenProps = [ "eventTime" .= epcisTimeJSON
            , "recordTime" .= Just epcisTimeJSON
            , "eventTimeZoneOffset" .= timeZoneJSON
            ]

why :: DWhy
why = DWhy (Just bizStep) (Just disposition)

whyProps :: [Pair]
whyProps = [ "bizStep" .= bizStepJSON
           , "disposition" .= dispositionJSON
           ]

where' :: DWhere
where' = DWhere (Just $ ReadPointLocation location)
                (Just $ BizLocation location)
                [ sourceLocation ]
                [ destinationLocation ]

whereProps :: [Pair]
whereProps = [ "sourceList" .= [ sourceLocationJSON ]
             , "destinationList" .= [ destinationLocationJSON ]
             , "readPoint" .= locationJSON
             , "bizLocation" .= locationJSON
             ]

mkAssetLabelEPC :: Text -> InstanceLabelEPC
mkAssetLabelEPC = GIAI (GS1CompanyPrefix "0614141") . SerialNumber

mkAssetLabelEPCJSON :: Text -> Value
mkAssetLabelEPCJSON = String . mappend "urn:epc:id:giai:0614141."

mkAssetLabel :: Text -> (InstanceLabelEPC, Value)
mkAssetLabel x = (mkAssetLabelEPC x, mkAssetLabelEPCJSON x)

(sensorLabel_1234, sensorLabelJSON_1234) = mkAssetLabel "1234"


mkReturnableAssetLabelEPC :: AssetType -> Text -> InstanceLabelEPC
mkReturnableAssetLabelEPC assetType serialNo = GRAI (GS1CompanyPrefix "0614141") assetType $ SerialNumber serialNo

mkReturnableAssetLabelEPCJSON :: Text -> Value
mkReturnableAssetLabelEPCJSON = String . mappend "urn:epc:id:grai:0614141.1."

mkReturnableAssetLabel :: Text -> (InstanceLabelEPC, Value)
mkReturnableAssetLabel x = (mkReturnableAssetLabelEPC (AssetType "1") x, mkReturnableAssetLabelEPCJSON x)

(palletLabel_1234, palletLabelJSON_1234) = mkReturnableAssetLabel "1234"


mkInstanceLabelEPC :: Text -> InstanceLabelEPC
mkInstanceLabelEPC = SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") . SerialNumber

mkInstanceLabelEPCJSON :: Text -> Value
mkInstanceLabelEPCJSON = String . mappend "urn:epc:id:sgtin:0614141.107346."

mkInstanceLabel :: Text -> (InstanceLabelEPC, Value)
mkInstanceLabel x = (mkInstanceLabelEPC x, mkInstanceLabelEPCJSON x)

(instanceLabelEPC_1234, instanceLabelEPCJSON_1234) = mkInstanceLabel "1234"
(instanceLabelEPC_5678, instanceLabelEPCJSON_5678) = mkInstanceLabel "5678"


mkClassLabelEPC :: Text -> ClassLabelEPC
mkClassLabelEPC = LGTIN (GS1CompanyPrefix "0614141") (ItemReference "107366") . Lot

mkClassLabelEPCJSON :: Text -> Value
mkClassLabelEPCJSON = String . mappend "urn:epc:class:lgtin:0614141.107366."

mkClassLabel :: Text -> (ClassLabelEPC, Value)
mkClassLabel x = (mkClassLabelEPC x, mkClassLabelEPCJSON x)

(classLabelEPC_321, classLabelEPCJSON_321) = mkClassLabel "321"
(classLabelEPC_654, classLabelEPCJSON_654) = mkClassLabel "654"


location :: LocationEPC
location = (SGLN (GS1CompanyPrefix "0614141") (LocationReference "07346") (Just (SGLNExtension "1234")))

locationJSON :: Value
locationJSON = String "urn:epc:id:sgln:0614141.07346.1234"

sourceLocation :: SourceLocation
sourceLocation = SourceLocation sourceOrDestType location

sourceLocationJSON :: Value
sourceLocationJSON = object [ "type" .= sourceOrDestTypeJSON
                            , "source" .= locationJSON
                            ]

destinationLocation :: DestinationLocation
destinationLocation = DestinationLocation sourceOrDestType location

destinationLocationJSON :: Value
destinationLocationJSON = object [ "type" .= sourceOrDestTypeJSON
                                 , "destination" .= locationJSON
                                 ]

timeZone :: TimeZone
timeZone = TimeZone 600 False ""

timeZoneJSON :: Value
timeZoneJSON = String "+10:00"

epcisTime :: EPCISTime
epcisTime = EPCISTime $ UTCTime (ModifiedJulianDay 58564) 0

epcisTimeJSON :: Value
epcisTimeJSON = String ( "2019-03-22T00:00:00Z" )

bizStep :: BizStep
bizStep = Accepting

bizStepJSON :: Value
bizStepJSON = String "urn:epcglobal:cbv:bizstep:accepting"

disposition :: Disposition
disposition = Damaged

dispositionJSON :: Value
dispositionJSON = String "urn:epcglobal:cbv:disp:damaged"

sourceOrDestType :: SourceDestType
sourceOrDestType = SDOwningParty

sourceOrDestTypeJSON :: Value
sourceOrDestTypeJSON = String "urn:epcglobal:cbv:sdt:owning_party"

bizTransaction :: BizTransaction
bizTransaction = BizTransaction Nothing Bol

bizTransactionJSON :: Value
bizTransactionJSON = object [ "bizTransaction" .= String "urn:epcglobal:cbv:btt:bol"
                            ]
