module Tests.Parser where

import           Data.Either.Combinators
import           Data.GS1.DWhat
import           Data.GS1.DWhen
import           Data.GS1.DWhere
import           Data.GS1.DWhy
import           Data.GS1.EPC
import           Data.GS1.Event
import           Data.GS1.EventID
import           Data.GS1.Parser.Parser
import           Data.Maybe
import           Data.Time
import           Data.UUID               as UUID
import           Test.Hspec
import           Text.XML
import           Text.XML.Cursor

testParser :: Spec
testParser = do

  describe "parse XML to obtain DWhen" $ do
    it "creates DWhen from valid XML" $ do
      doc <- Text.XML.readFile def "test/test-xml/ObjectEvent.xml"
      let cursor = fromDocument doc
      let oeCursors = getCursorsByName "ObjectEvent" cursor
      let et = "2005-04-03T20:33:31.116-06:00"
      let et1 = "2005-04-04T20:33:31.116-06:00"
      let tzStr = "-06:00"
      let t = parseStr2Time et
      let t1 = parseStr2Time et1
      let tz = parseStr2TimeZone tzStr
      let tz1 = parseStr2TimeZone tzStr
      parseDWhen <$> oeCursors `shouldBe`
        [Right (DWhen (fromRight' t) (Just (fromRight' t)) (fromRight' tz)),
        Right (DWhen (fromRight' t1) Nothing (fromRight' tz1))]

    it "Returns ParseFailure from ObjectEvent XML without Event Time" $ do
      doc <- Text.XML.readFile def "test/test-xml/ObjectEventNoEventTime.xml"
      let cursor = fromDocument doc
      let oeCursors = getCursorsByName "ObjectEvent" cursor
      parseDWhen <$> oeCursors `shouldBe` [Left $ TimeZoneError (XMLSnippet "")]

  describe "parse DWhen AggregationEvent" $
    it "create DWhen from AggregationEvent" $ do
      doc <- Text.XML.readFile def "test/test-xml/AggregationEvent.xml"
      let cursor = fromDocument doc
      let oeCursors = getCursorsByName "AggregationEvent" cursor
      parseDWhen <$> oeCursors `shouldBe`
        [Right
          (
            DWhen
              (EPCISTime $ read "2013-06-08 12:58:56.591")
              Nothing
              (read "+02:00" :: TimeZone)
          )
        ]

  describe "parse XML to obtain Action" $
    it "finds action from Single ObjectEventNoEventTime XML" $ do
      doc <- Text.XML.readFile def "test/test-xml/ObjectEventNoEventTime.xml"
      let cursor = fromDocument doc
      parseAction cursor `shouldBe` Right Observe

  describe "parse XML to obtain EPC List" $ do
    it "finds all epcs" $ do
      doc <- Text.XML.readFile def "test/test-xml/ObjectEvent2.xml"
      let cursor = fromDocument doc
      parseLabelEPCs "epcList" "quantityList" cursor
        `shouldBe`
          [
            Right $ IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2017"),
            Right $ IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018"),
            Right $ CL (LGTIN (GS1CompanyPrefix "4012345") (ItemReference "012345") (Lot "998877"))
                (Just $ MeasuredQuantity (Amount 200) (Uom "KGM"))
          ]

    it "finds all child epcs" $ do
      doc <- Text.XML.readFile def "test/test-xml/AggregationEvent.xml"
      let cursor = fromDocument doc
      parseLabelEPCs "childEPCs" "childQuantityList" cursor
        `shouldBe`
          [
            Right $ IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2017"),
            Right $ IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018"),
            Right $ CL (CSGTIN (GS1CompanyPrefix "4012345") Nothing (ItemReference "098765"))
                (Just $ ItemCount 10),
            Right $ CL (LGTIN (GS1CompanyPrefix "4012345") (ItemReference "012345") (Lot "998877"))
                (Just $ MeasuredQuantity (Amount 200.5) (Uom "KGM"))
          ]

  describe "parse XML to get BizStep" $ do
    it "find all the BizStep in multiple events XML" $ do
      doc <- Text.XML.readFile def "test/test-xml/ObjectEvent.xml"
      let cursor = fromDocument doc
      parseBizStep cursor `shouldBe` Right Shipping

    it "find the first BizStep in single Event XML" $ do
      doc2 <- Text.XML.readFile def "test/test-xml/ObjectEvent2.xml"
      let cursor = fromDocument doc2
      parseBizStep cursor `shouldBe` Right Shipping

  describe "parse XML to get Disposition" $
    it "find all the Disposition in single XML" $ do
      doc2 <- Text.XML.readFile def "test/test-xml/ObjectEvent2.xml"
      let cursor = fromDocument doc2
      parseDisposition cursor `shouldBe` Right InTransit

  describe "parse DWhy" $ do
    it "ObjectEvent2.xml" $ do
      doc2 <- Text.XML.readFile def "test/test-xml/ObjectEvent2.xml"
      let cursor = fromDocument doc2
      let oeCursors = getCursorsByName "ObjectEvent" cursor
      parseDWhy <$> oeCursors `shouldBe`
        [Right $ DWhy (Just Shipping) (Just InTransit)]

    it "Transformation.xml" $ do
      doc2 <- Text.XML.readFile def "test/test-xml/TransformationEvent.xml"
      let cursor = fromDocument doc2
      let teCursors = cursor $// element "TransformationEvent"
      parseDWhy <$> teCursors `shouldBe`
        [Right $ DWhy (Just Commissioning) (Just InProgress)]

    it "Empty" $ do
      doc2 <- Text.XML.readFile def "test/test-xml/Empty_DWhy.xml"
      let cursor = fromDocument doc2
      let teCursors = cursor $// element "TransformationEvent"
      parseDWhy <$> teCursors `shouldBe`
        [Right $ DWhy Nothing Nothing]

  describe "parse DWhy from AggregationEvent" $
    it "create DWhy from AggregationEvent" $ do
      doc <- Text.XML.readFile def "test/test-xml/AggregationEvent.xml"
      let cursor = fromDocument doc
      let oeCursors = getCursorsByName "AggregationEvent" cursor
      parseDWhy <$> oeCursors `shouldBe`
        [Right (DWhy (Just Receiving) (Just InProgress))]

  -- the Nothing extension used because a value of 0 indicates no extension
  describe "parse XML to obtain DWhere" $
    it "ObjectEvent" $ do
      doc2 <- Text.XML.readFile def "test/test-xml/ObjectEvent.xml"
      let cursor = fromDocument doc2
      let oeCursors = getCursorsByName "ObjectEvent" cursor
      parseDWhere <$> oeCursors `shouldBe`
        [Right DWhere {
          _readPoint =
              [ReadPointLocation $ SGLN (GS1CompanyPrefix "0614141") (LocationReference "07346") (Just $ SGLNExtension "1234")]
          , _bizLocation = []
          , _srcType = []
          , _destType = []
        }, Right DWhere {
          _readPoint =
              [ReadPointLocation $ SGLN (GS1CompanyPrefix "0012345") (LocationReference "11111") (Just $ SGLNExtension "400")]
          , _bizLocation =
              [BizLocation $ SGLN (GS1CompanyPrefix "0012345") (LocationReference "11111") Nothing]
          , _srcType = []
          , _destType = []
        }]

  describe "parse DWhere from AggregationEvent" $
    it "create DWhere from AggregationEvent" $ do
      doc <- Text.XML.readFile def "test/test-xml/AggregationEvent.xml"
      let cursor = fromDocument doc
      let oeCursors = getCursorsByName "AggregationEvent" cursor
      parseDWhere <$> oeCursors `shouldBe`
        -- NOTE that according to EPCIS_Guideline.pdf page 35,
        -- a plain GLN indicated by extension valued 0
        [Right
          DWhere {
            _readPoint =
                [ReadPointLocation $ SGLN (GS1CompanyPrefix "0614141") (LocationReference "00777") Nothing],
            _bizLocation =
                [BizLocation $ SGLN (GS1CompanyPrefix "0614141") (LocationReference "00888") Nothing],
            _srcType = [],
            _destType = []
          }]

  describe "parse QuantityElement" $
    it "parses quantity elements" $ do
      doc <- Text.XML.readFile def "test/test-xml/ObjectEvent2.xml"
      let cursor = fromDocument doc
      let oeCursors = getCursorsByName "quantityElement" cursor
      parseQuantity <$> oeCursors `shouldBe` [Just $ MeasuredQuantity (Amount 200) (Uom "KGM")]

  describe "parse BizTransaction" $ do
    it "parse BizTransaction element" $ do
      doc <- Text.XML.readFile def "test/test-xml/ObjectEvent.xml"
      let cursor = fromDocument doc
      let btCursor = cursor $// element "bizTransactionList"
      parseBizTransaction <$> btCursor `shouldBe`
        [
          [
            Right BizTransaction {
              _btid = BizTransactionID "http://transaction.acme.com/po/12345678",
              _bt = Po
              }
          ],
          [
            Right BizTransaction {
              _btid = BizTransactionID "http://transaction.acme.com/po/12345678",
              _bt = Po},
            Right BizTransaction {
              _btid = BizTransactionID "urn:epcglobal:cbv:bt:0614141073467:1152",
              _bt = Desadv
            }
          ]
        ]

    it "get all attrs" $ do
      doc2 <- Text.XML.readFile def "test/test-xml/ObjectEvent.xml"
      let cursor = fromDocument doc2
      let btCursor = cursor $// element "bizTransactionList"
      let c = head btCursor
      let attrs = c $/ element "bizTransaction" &| attribute "type"
      readURI <$> foldMap id attrs `shouldBe` [Right Po]

  describe "parse DWhat" $ do
    describe "Parse ObjectDWhat" $ do
      it "ObjectEvent2" $ do
        doc <- Text.XML.readFile def "test/test-xml/ObjectEvent2.xml"
        let cursor = fromDocument doc
        let oeCursors = getCursorsByName "ObjectEvent" cursor
        parseObjectDWhat <$> oeCursors `shouldBe`
          [
            Right $ ObjectDWhat Observe
              [
                IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2017"),
                IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018"),
                CL (LGTIN (GS1CompanyPrefix "4012345") (ItemReference "012345") (Lot "998877"))
                    (Just $ MeasuredQuantity (Amount 200) (Uom "KGM"))
              ]
          ]

      it "ObjectEvent_11_TrainArrives.xml" $ do
        doc <- Text.XML.readFile def "test/test-xml/ObjectEvent_11_TrainArrives.xml"
        let cursor = fromDocument doc
        let oeCursors = getCursorsByName "ObjectEvent" cursor
        parseObjectDWhat <$> oeCursors `shouldBe`
          [ Right $ ObjectDWhat Observe [] ]

      it "ObjectEventNoEventTime.xml" $ do
        doc <- Text.XML.readFile def "test/test-xml/ObjectEventNoEventTime.xml"
        let cursor = fromDocument doc
        let oeCursors = getCursorsByName "ObjectEvent" cursor
        parseObjectDWhat <$> oeCursors `shouldBe`
          [
            Right $ ObjectDWhat Observe
              [
                IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2017"),
                IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018")
              ]
          ]

      it "ObjectEvent.xml" $ do
        doc <- Text.XML.readFile def "test/test-xml/ObjectEvent.xml"
        let cursor = fromDocument doc
        let oeCursors = getCursorsByName "ObjectEvent" cursor
        parseObjectDWhat <$> oeCursors `shouldBe`
          [
            Right $ ObjectDWhat Observe
              [
                IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2017"),
                IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018")
              ],
            Right $ ObjectDWhat Observe
              [IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018")]
          ]

    it "parses a valid AggregationDWhat" $ do
      doc <- Text.XML.readFile def "test/test-xml/AggregationEvent.xml"
      let cursor = fromDocument doc
      let aeCursors = getCursorsByName "AggregationEvent" cursor
      parseAggregationDWhat <$> aeCursors `shouldBe`
        [
          Right $ AggregationDWhat Observe (Just . ParentLabel $ SSCC (GS1CompanyPrefix "0614141") (SerialNumber "1234567890"))
            [
              IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2017"),
              IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018"),
              CL (CSGTIN (GS1CompanyPrefix "4012345") Nothing (ItemReference "098765")) (Just $ ItemCount 10),
              CL (LGTIN (GS1CompanyPrefix "4012345") (ItemReference "012345") (Lot "998877"))
                  (Just $ MeasuredQuantity (Amount 200.5) (Uom "KGM"))
            ]
        ]

    it "parses a valid TransactionDWhat" $ do
      doc <- Text.XML.readFile def "test/test-xml/TransactionEvent.xml"
      let cursor = fromDocument doc
      let teCursors = getCursorsByName "TransactionEvent" cursor
      parseTransactionDWhat <$> teCursors `shouldBe`
        [Right (TransactionDWhat Observe Nothing
          [BizTransaction {
            _btid = BizTransactionID "http://transaction.acme.com/po/12345678",
            _bt = Po}]
          [IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2017"),
          IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018")]),
        Right (TransactionDWhat Observe Nothing
          [BizTransaction {
            _btid = BizTransactionID "http://transaction.acme.com/po/12345678",
            _bt = Po},
          BizTransaction {
            _btid = BizTransactionID "urn:epcglobal:cbv:bt:0614141073467:1152",
            _bt = Desadv}]
          [IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018")])]

    describe "run parseTransformationDWhat" $
      it "parses valid DWhat" $ do
        doc <- Text.XML.readFile def "test/test-xml/TransformationEvent.xml"
        let cursor = fromDocument doc
        let tCursors = getCursorsByName "TransformationEvent" cursor
        parseTransformationDWhat <$> tCursors `shouldBe`
          [
            Right $ TransformationDWhat Nothing
              (InputEPC <$>
              [ IL (SGTIN (GS1CompanyPrefix "4012345") Nothing (ItemReference "011122") (SerialNumber "25"))
              , IL (SGTIN (GS1CompanyPrefix "4000001") Nothing (ItemReference "065432") (SerialNumber "99886655"))
              , CL (LGTIN (GS1CompanyPrefix "4012345") (ItemReference "011111") (Lot "4444")) (Just (MeasuredQuantity (Amount 10.0) (Uom "KGM")))
              , CL (LGTIN (GS1CompanyPrefix "0614141") (ItemReference "077777") (Lot "987")) (Just (ItemCount 30))
              , CL (CSGTIN (GS1CompanyPrefix "4012345") Nothing (ItemReference "066666")) (Just (ItemCount 220))
              ])
              (OutputEPC <$>
              [
                IL (SGTIN (GS1CompanyPrefix "4012345") Nothing (ItemReference "077889") (SerialNumber "25")),
                IL (SGTIN (GS1CompanyPrefix "4012345") Nothing (ItemReference "077889") (SerialNumber "26")),
                IL (SGTIN (GS1CompanyPrefix "4012345") Nothing (ItemReference "077889") (SerialNumber "27")),
                IL (SGTIN (GS1CompanyPrefix "4012345") Nothing (ItemReference "077889") (SerialNumber "28"))
              ])
          ]


  describe "Parse Full Events" $ do
    describe "Object Events" $ do
      it "ObjectEvent2" $ do
        doc <- Text.XML.readFile def "test/test-xml/ObjectEvent2.xml"
        let cursor = fromDocument doc
        let parsedEvents = parseEventByType cursor ObjectEventT
        parsedEvents`shouldBe`
          -- a huge dwhat element
          [Right $ Event
            ObjectEventT -- type
            (Just (EventID (fromJust $
                fromString "b1080b06-e9cc-11e6-bf0e-fe55135034f3")))
            -- eid
            -- a dwhat element
            (
              ObjWhat $ ObjectDWhat Observe
              [
                IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2017"),
                IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018"),
                CL (LGTIN (GS1CompanyPrefix "4012345") (ItemReference "012345") (Lot "998877"))
                    (Just $ MeasuredQuantity (Amount 200) (Uom "KGM"))
              ]
            )
            -- a dwhen element
            (
              DWhen
                (EPCISTime $ read "2005-04-03 20:33:31.116-06:00")
                Nothing
                (read "-06:00" :: TimeZone)
            )
            -- a dwhy element
            (DWhy (Just Shipping) (Just InTransit))
            -- a dwhere element
            (
              DWhere
              [ReadPointLocation $ SGLN (GS1CompanyPrefix "0614141") (LocationReference "00777") Nothing]
              -- [ReadPointLocation]
              [BizLocation $ SGLN (GS1CompanyPrefix "0614141") (LocationReference "00888") Nothing]
              -- [BizLocation]
              [
                SrcDestLocation (
                  SDPossessingParty, -- SourceDestType
                  SGLN (GS1CompanyPrefix "4012345") (LocationReference "00001") Nothing
                  -- LocationEPC
                )
              ] -- srcType
              [
                SrcDestLocation (
                  SDOwningParty,
                  SGLN (GS1CompanyPrefix "0614141") (LocationReference "00001") Nothing
                ),
                SrcDestLocation (
                  SDLocation,
                  SGLN (GS1CompanyPrefix "0614141") (LocationReference "00777") Nothing
                )
              ] -- destType
            )
          ]

      it "ObjectEvent" $ do
        doc <- Text.XML.readFile def "test/test-xml/ObjectEvent.xml"
        let cursor = fromDocument doc
        let parsedEvents = parseEventByType cursor ObjectEventT
        -- this is a template to write tests for the events
        parsedEvents`shouldBe`
              -- a huge dwhat element
          [
            Right $ Event
              ObjectEventT -- type
              -- eid
              (Just (EventID (fromJust $
                  fromString "b1080840-e9cc-11e6-bf0e-fe55135034f3")))
              -- a dwhat element
              (
                ObjWhat $ ObjectDWhat Observe
                [
                  IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2017"),
                  IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018")
                ]
              )
              -- a dwhen element
              (
                DWhen
                  (EPCISTime $ read "2005-04-03 20:33:31.116-06:00")
                  (Just (EPCISTime $ read "2005-04-03 20:33:31.116-06:00"))
                  (read "-06:00" :: TimeZone)
              )
              -- a dwhy element
              (DWhy (Just Shipping) (Just InTransit))
              -- a dwhere element
              -- <id>urn:epc:id:sgln:0614141.07346.1234</id>
              (
                DWhere
                  [ReadPointLocation $ SGLN (GS1CompanyPrefix "0614141") (LocationReference "07346") (Just (SGLNExtension "1234"))]
                  [] [] []
              ),

            -- second element
            Right $ Event
              ObjectEventT -- type
              -- eid
              (Just (EventID (fromJust $
                  fromString "b108094e-e9cc-11e6-bf0e-fe55135034f3")))
              -- a dwhat element
              (
                ObjWhat $ ObjectDWhat Observe
                  [IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018")]
              )
              -- a dwhen element
              (
                DWhen
                  (EPCISTime $ read "2005-04-04 20:33:31.116-06:00")
                  Nothing
                  (read "-06:00" :: TimeZone)
              )
              -- a dwhy element
              (DWhy (Just Receiving) (Just InProgress))
              -- a dwhere element
              (
                DWhere
                [ReadPointLocation $ SGLN (GS1CompanyPrefix "0012345") (LocationReference "11111") (Just (SGLNExtension "400"))]
                -- [ReadPointLocation]
                [BizLocation $ SGLN (GS1CompanyPrefix "0012345") (LocationReference "11111") Nothing]
                -- [BizLocation]
                [] []
              )
          ]

    it "parses a valid transformation event" $ do
      doc <- Text.XML.readFile def "test/test-xml/TransformationEvent.xml"
      let cursor = fromDocument doc
      let parsedEvents = parseEventByType cursor TransformationEventT
      -- this is a template to write tests for the events
      parsedEvents`shouldBe`
            -- a huge dwhat element
        [Right $ Event
          TransformationEventT -- type
          Nothing -- eid
          -- a dwhat element
          (
            TransformWhat $ TransformationDWhat Nothing
              (InputEPC <$> [
                IL (SGTIN (GS1CompanyPrefix "4012345") Nothing (ItemReference "011122") (SerialNumber "25")),
                IL (SGTIN (GS1CompanyPrefix "4000001") Nothing (ItemReference "065432") (SerialNumber "99886655")),
                CL (LGTIN (GS1CompanyPrefix "4012345") (ItemReference "011111") (Lot "4444"))
                    (Just (MeasuredQuantity (Amount 10.0) (Uom "KGM"))),
                CL (LGTIN (GS1CompanyPrefix "0614141") (ItemReference "077777") (Lot "987")) (Just (ItemCount 30)),
                CL (CSGTIN (GS1CompanyPrefix "4012345") Nothing (ItemReference "066666")) (Just (ItemCount 220))
              ])
              (OutputEPC <$> [
                IL (SGTIN (GS1CompanyPrefix "4012345") Nothing (ItemReference "077889") (SerialNumber "25")),
                IL (SGTIN (GS1CompanyPrefix "4012345") Nothing (ItemReference "077889") (SerialNumber "26")),
                IL (SGTIN (GS1CompanyPrefix "4012345") Nothing (ItemReference "077889") (SerialNumber "27")),
                IL (SGTIN (GS1CompanyPrefix "4012345") Nothing (ItemReference "077889") (SerialNumber "28"))
              ])
          )
          -- a dwhen element
          (
            DWhen
              (EPCISTime $ read "2013-10-31 14:58:56.591Z") -- eventTime
              Nothing -- recordTime
              (read "+02:00" :: TimeZone) -- timeZone
          )
          -- a dwhy element
          (DWhy (Just Commissioning) (Just InProgress))
          -- a dwhere element
          (
            DWhere
            [ReadPointLocation $ SGLN (GS1CompanyPrefix "4012345") (LocationReference "00001") Nothing]
            -- [ReadPointLocation]
            [] -- [BizLocation]
            [] -- srcType
            [] -- destType
          )
        ]

    it "parses a valid transaction event" $ do
      doc <- Text.XML.readFile def "test/test-xml/TransactionEvent.xml"
      let cursor = fromDocument doc
      let parsedEvents = parseEventByType cursor TransactionEventT
      -- this is a template to write tests for the events
      parsedEvents`shouldBe`
            -- a huge dwhat element
        [Right $ Event
          TransactionEventT -- type
          (Just (EventID (fromJust $
              fromString "b1080840-e9cc-11e6-bf0e-fe55135034f3")))
          -- eid
          -- a dwhat element
          (
            TransactWhat $ TransactionDWhat Observe
            Nothing
            [
              BizTransaction{_btid= BizTransactionID "http://transaction.acme.com/po/12345678", _bt=Po}
            ]
            [
              IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2017"),
              IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018")
            ]
          )
          -- a dwhen element
          (
            DWhen
              (EPCISTime $ read "2005-04-03 20:33:31.116-06:00")
              (Just (EPCISTime $ read "2005-04-03 20:33:31.116-06:00"))
              (read "-06:00" :: TimeZone)
          )
          -- a dwhy element
          (DWhy (Just Shipping) (Just InTransit))
          -- a dwhere element
          (
            DWhere
            [ReadPointLocation $ SGLN (GS1CompanyPrefix "0614141") (LocationReference "07346")  (Just (SGLNExtension "1234"))]
            -- [ReadPointLocation]
            []-- [BizLocation]
            [] -- srcType
            [] -- destType
          ),

          -- second element
          Right $ Event
          TransactionEventT -- type
          (Just (EventID (fromJust $
              fromString "b108094e-e9cc-11e6-bf0e-fe55135034f3")))
          -- eid
          -- a dwhat element
          (
            TransactWhat $ TransactionDWhat Observe
            Nothing
            [
              BizTransaction{_btid= BizTransactionID "http://transaction.acme.com/po/12345678", _bt=Po},
              BizTransaction{_btid= BizTransactionID "urn:epcglobal:cbv:bt:0614141073467:1152", _bt=Desadv}
            ]
            [IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018")]
          )
          -- a dwhen element
          (
            DWhen
              (EPCISTime $ read "2005-04-04 20:33:31.116-06:00")
              Nothing
              (read "-06:00" :: TimeZone)
          )
          -- a dwhy element
          (DWhy (Just Receiving) (Just InProgress))
          -- a dwhere element
          (
            DWhere
            [ReadPointLocation $ SGLN (GS1CompanyPrefix "0012345") (LocationReference "11111")  (Just (SGLNExtension "400"))]
            -- [ReadPointLocation]
            [BizLocation $ SGLN (GS1CompanyPrefix "0012345") (LocationReference "11111") Nothing]
            -- [BizLocation]
            [] -- srcType
            [] -- destType
          )
        ]

    it "parses a valid aggregation event" $ do
      doc <- Text.XML.readFile def "test/test-xml/AggregationEvent.xml"
      let cursor = fromDocument doc
      let parsedEvents = parseEventByType cursor AggregationEventT
      parsedEvents`shouldBe`
            -- a huge dwhat element
        [Right $ Event
          AggregationEventT -- type
          (Just (EventID (fromJust $
              fromString "b1080840-e9cc-11e6-bf0e-fe55240134d5"))) -- eid
          -- a dwhat element
          (
            AggWhat $ AggregationDWhat Observe
            (Just (ParentLabel $ SSCC (GS1CompanyPrefix "0614141") (SerialNumber "1234567890"))) -- Maybe ParentLabel
            [
              IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2017"),
              IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018"),
              CL (CSGTIN (GS1CompanyPrefix "4012345") Nothing (ItemReference "098765")) (Just $ ItemCount 10),
              CL (LGTIN (GS1CompanyPrefix "4012345") (ItemReference "012345") (Lot "998877"))
                  (Just $ MeasuredQuantity (Amount 200.5) (Uom "KGM"))
            ] -- [LabelEPC]
          )
          -- a dwhen element
          (
            DWhen
              (EPCISTime $ read "2013-06-08 14:58:56.591+02:00")
              Nothing
              (read "+02:00" :: TimeZone)
          )
          -- a dwhy element
          (DWhy (Just Receiving) (Just InProgress))
          -- a dwhere element
          (
            DWhere
            [ReadPointLocation $ SGLN (GS1CompanyPrefix "0614141") (LocationReference "00777") Nothing]
            -- [ReadPointLocation]
            [BizLocation $ SGLN (GS1CompanyPrefix "0614141") (LocationReference "00888") Nothing]
            -- [BizLocation]
            [] -- srcType
            [] -- destType
          )
        ]


  describe "test some basic functions in Parser" $ do
    it "parseSingleElem invalid" $
      parseSingleElem "Invalid Test Tag" Right []
        `shouldBe`
          (Left $ TagNotFound (MissingTag "Invalid Test Tag"))
    it "parseSingleElem valid" $
      parseSingleElem "Invalid Test Tag" Right ["hi"]
        `shouldBe` (Right "hi")

    it "parseTimeXML invalid" $
      parseTimeXML "Invalid Test Tag" []
        `shouldBe`
          (Left $ TagNotFound (MissingTag "Invalid Test Tag"))
    it "parseTimeXML invalid 2" $
      parseTimeXML "Invalid Test Tag"
        ["the quick brown fox jumped over the lazy dog", "2005-04-03T20:33:31.116-06:00"]
          `shouldBe`
            (Left $ TimeZoneError (XMLSnippet "the quick brown fox jumped over the lazy dog"))
    it "parseTimeXML valid" $
      parseTimeXML "Invalid Test Tag" ["2005-04-03T20:33:31.116-06:00", "the quick brown fox jumped over the lazy dog"]
        `shouldBe`
          Right (EPCISTime $ read "2005-04-03 20:33:31.116-06:00")

    it "parseTimeZoneXML invalid" $
      parseTimeZoneXML "Invalid Test Tag" []
        `shouldBe`
          (Left $ TagNotFound (MissingTag "Invalid Test Tag"))
    it "parseTimeZoneXML invalid 2" $
      parseTimeZoneXML "Invalid Test Tag" ["the quick brown fox jumped over the lazy dog", "2005-04-03T20:33:31.116-06:00"]
        `shouldBe`
          (Left $ TimeZoneError (XMLSnippet "the quick brown fox jumped over the lazy dog"))
    it "parseTimeZoneXML valid" $
      parseTimeZoneXML "Invalid Test Tag"
        ["-06:00", "the quick brown fox jumped over the lazy dog"]
          `shouldBe`
            Right (read "-06:00" :: TimeZone)
