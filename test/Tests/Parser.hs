{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TemplateHaskell #-}

module Tests.Parser where

import           Test.Hspec
import           Text.XML
import           Text.XML.Cursor

import qualified Data.Text              as T
import           Data.Time.LocalTime
import           Data.Either.Combinators

import           Data.GS1.DWhat
import           Data.GS1.DWhen
import           Data.GS1.DWhere
import           Data.GS1.DWhy
import           Data.GS1.EPC
import           Data.GS1.Event
import           Data.GS1.Parser.Parser
import           Data.GS1.EventID
import           Data.UUID as UUID
import           Data.Maybe
import           Data.Time

testParser :: Spec
testParser = do

  describe "parse XML to obtain DWhen" $ do
    it "creates DWhen from valid XML" $ do
      doc <- Text.XML.readFile def "test/test-xml/ObjectEvent.xml"
      let cursor = fromDocument doc
      let oeCursors = getCursorsByName "ObjectEvent" cursor
      --mapM_ print $ parseDWhen <$> oeCursors
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

    it "creates Nothing from Single ObjectEvent XML without Event Time" $ do
      doc <- Text.XML.readFile def "test/test-xml/ObjectEventNoEventTime.xml"
      let cursor = fromDocument doc
      let oeCursors = getCursorsByName "ObjectEvent" cursor
      parseDWhen <$> oeCursors `shouldBe` [Left TimeZoneError]

  
  -- TODO = fix issue in Parser.hs
  -- PROBLEM NOTICED IN Parser.hs src = in parseDWhen, it assumes that parseTimeZoneXML' produces a Just value into value tz
  -- .. add DWhen test for Aggregation

    -- it "AggregationEvent" $ do
    --   error "@todo Add DWhen test for Aggregation"
  describe "parse DWhen AggregationEvent" $
    it "create DWhen from AggregationEvent" $ do
      doc <- Text.XML.readFile def "test/test-xml/AggregationEvent.xml"
      let cursor = fromDocument doc
      let oeCursors = getCursorsByName "AggregationEvent" cursor  
      parseDWhen <$> oeCursors `shouldBe`
        [Right (DWhen (read "2013-06-08 12:58:56.591" :: UTCTime) Nothing (read "+02:00" :: TimeZone))]

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
            Right $ IL $ SGTIN "0614141" Nothing "107346" "2017",
            Right $ IL $ SGTIN "0614141" Nothing "107346" "2018",
            Right $ CL (LGTIN "4012345" "012345" "998877")
                (Just $ MeasuredQuantity 200 "KGM")
          ]

    it "finds all child epcs" $ do
      doc <- Text.XML.readFile def "test/test-xml/AggregationEvent.xml"
      let cursor = fromDocument doc
      parseLabelEPCs "childEPCs" "childQuantityList" cursor
        `shouldBe`
          [
            Right $ IL $ SGTIN "0614141" Nothing "107346" "2017",
            Right $ IL $ SGTIN "0614141" Nothing "107346" "2018",
            Right $ CL (CSGTIN "4012345" Nothing "098765")
                (Just $ ItemCount 10),
            Right $ CL (LGTIN "4012345" "012345" "998877")
                (Just $ MeasuredQuantity 200.5 "KGM")
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

    -- @todo create a dummy xml with no disposition or bizStep
    -- it "AggregationEvent" $ do
    --   error "@todo Add DWhy test for Aggregation"
  describe "parse DWhy from AggregationEvent" $ do
    it "create DWhy from AggregationEvent" $ do
      doc <- Text.XML.readFile def "test/test-xml/AggregationEvent.xml"
      let cursor = fromDocument doc
      let oeCursors = getCursorsByName "AggregationEvent" cursor
      parseDWhy <$> oeCursors `shouldBe`
        [Right (DWhy (Just Receiving) (Just InProgress))]

  -- the Nothing extension used because a value of 0 indicates no extension
  describe "parse XML to obtain DWhere" $ do
    it "ObjectEvent" $ do
      doc2 <- Text.XML.readFile def "test/test-xml/ObjectEvent.xml"
      let cursor = fromDocument doc2
      let oeCursors = getCursorsByName "ObjectEvent" cursor
      --mapM_ print $ parseDWhere <$> oeCursors
      parseDWhere <$> oeCursors `shouldBe`
        [Right DWhere {
          _readPoint =
              [SGLN "0614141" (LocationReferenceNum "07346") (Just "1234")]
          , _bizLocation = []
          , _srcType = []
          , _destType = []
        }, Right DWhere {
          _readPoint =
              [SGLN "0012345" (LocationReferenceNum "11111") (Just "400")]
          , _bizLocation =
              [SGLN "0012345" (LocationReferenceNum "11111") Nothing]
          , _srcType = []
          , _destType = []
        }]

    -- it "AggregationEvent" $ do
    --   error "@todo Add DWhere test for Aggregation"
  describe "parse DWhere from AggregationEvent" $ do
    it "create DWhere from AggregationEvent" $ do
      doc <- Text.XML.readFile def "test/test-xml/AggregationEvent.xml"
      let cursor = fromDocument doc
      let oeCursors = getCursorsByName "AggregationEvent" cursor
      parseDWhere <$> oeCursors `shouldBe`
        -- NOTE that according to EPCIS_Guideline.pdf page 35, a plain GLN indicated by extension valued 0
        [Right (DWhere{_readPoint=[SGLN "0614141" (LocationReferenceNum "00777") Nothing],
                       _bizLocation=[SGLN "0614141" (LocationReferenceNum "00888") Nothing],
                       -- TODO = check these should be empty
                      _srcType=[],
                      _destType=[]})]
    
  describe "parse QuantityElement" $
    it "parses quantity elements" $ do
      doc <- Text.XML.readFile def "test/test-xml/ObjectEvent2.xml"
      let cursor = fromDocument doc
      let oeCursors = getCursorsByName "quantityElement" cursor
      parseQuantity <$> oeCursors `shouldBe` [Just $ MeasuredQuantity 200 "KGM"]

      -- TODO = check... may be incorrect!
  describe "parse BizTransaction" $ do
    it "parse BizTransaction element" $ do
      doc <- Text.XML.readFile def "test/test-xml/ObjectEvent.xml"
      let cursor = fromDocument doc
      let btCursor = cursor $// element "bizTransactionList"
      parseBizTransaction <$> btCursor `shouldBe`
        [
          [
            Right BizTransaction {
              _btid = "http://transaction.acme.com/po/12345678",
              _bt = Po
              }
          ],
          [
            Right BizTransaction {
              _btid = "http://transaction.acme.com/po/12345678",
              _bt = Po},
            Right BizTransaction {
              _btid = "urn:epcglobal:cbv:bt:0614141073467:1152",
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
      readURI . T.unpack <$> foldMap id attrs `shouldBe` [Right Po]

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
                IL $ SGTIN "0614141" Nothing "107346" "2017",
                IL $ SGTIN "0614141" Nothing "107346" "2018",
                CL (LGTIN "4012345" "012345" "998877")
                    (Just $ MeasuredQuantity 200 "KGM")
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
                IL $ SGTIN "0614141" Nothing "107346" "2017",
                IL $ SGTIN "0614141" Nothing "107346" "2018"
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
                IL $ SGTIN "0614141" Nothing "107346" "2017",
                IL $ SGTIN "0614141" Nothing "107346" "2018"
              ],
            Right $ ObjectDWhat Observe
              [IL $ SGTIN "0614141" Nothing "107346" "2018"]
          ]

    it "parses a valid AggregationDWhat" $ do
      doc <- Text.XML.readFile def "test/test-xml/AggregationEvent.xml"
      let cursor = fromDocument doc
      let aeCursors = getCursorsByName "AggregationEvent" cursor
      -- TODO = check Nothings are appropriate below
      parseAggregationDWhat <$> aeCursors `shouldBe`
        [
          Right $ AggregationDWhat Observe (Just $ SSCC "0614141" "1234567890")
            [
              IL $ SGTIN "0614141" Nothing "107346" "2017",
              IL $ SGTIN "0614141" Nothing "107346" "2018",
              CL (CSGTIN "4012345" Nothing "098765") (Just $ ItemCount 10),
              CL (LGTIN "4012345" "012345" "998877")
                  (Just $ MeasuredQuantity 200.5 "KGM")
            ]
        ]

    -- TODO = check... may be incorrect!
    it "parses a valid TransactionDWhat" $ do
      doc <- Text.XML.readFile def "test/test-xml/TransactionEvent.xml"
      let cursor = fromDocument doc
      let teCursors = getCursorsByName "TransactionEvent" cursor
      -- TODO = check Nothings are appropriate in below
      parseTransactionDWhat <$> teCursors `shouldBe`
        [Right (TransactionDWhat Observe Nothing
          [BizTransaction {
            _btid = "http://transaction.acme.com/po/12345678",
            _bt = Po}]
          [IL $ SGTIN "0614141" Nothing "107346" "2017",
          IL $ SGTIN "0614141" Nothing "107346" "2018"]),
        Right (TransactionDWhat Observe Nothing
          [BizTransaction {
            _btid = "http://transaction.acme.com/po/12345678",
            _bt = Po},
          BizTransaction {
            _btid = "urn:epcglobal:cbv:bt:0614141073467:1152",
            _bt = Desadv}]
          [IL $ SGTIN "0614141" Nothing "107346" "2018"])]

    describe "run parseTransformationDWhat" $
      it "parses valid DWhat" $ do
        doc <- Text.XML.readFile def "test/test-xml/TransformationEvent.xml"
        let cursor = fromDocument doc
        let tCursors = getCursorsByName "TransformationEvent" cursor
        parseTransformationDWhat <$> tCursors `shouldBe`
          [ 
            Right $ TransformationDWhat Nothing
              [
                IL (SGTIN "4012345" Nothing "011122" "25"),
                IL (SGTIN "4000001" Nothing "065432" "99886655"),
                CL (LGTIN "4012345" "011111" "4444")
                    (Just (MeasuredQuantity 10.0 "KGM")),
                CL (LGTIN "0614141" "077777" "987") (Just (ItemCount 30)),
                CL (CSGTIN "4012345" Nothing "066666") (Just (ItemCount 220))
              ] 
              [
                IL (SGTIN "4012345" Nothing "077889" "25"),
                IL (SGTIN "4012345" Nothing "077889" "26"),
                IL (SGTIN "4012345" Nothing "077889" "27"),
                IL (SGTIN "4012345" Nothing "077889" "28")
              ]
          ]


  describe "Parse Full Events" $ do
    describe "Object Events" $ do
      it "ObjectEvent2" $ do
        doc <- Text.XML.readFile def "test/test-xml/ObjectEvent2.xml"
        let cursor = fromDocument doc
        let parsedEvents = parseEventByType cursor ObjectEventT
        -- this is a template to write tests for the events
        parsedEvents`shouldBe`
              -- a huge dwhat element
          [Right $ Event
            -- @todo annonate the attributes with comments about what they are
            ObjectEventT -- type
            (Just (EventID (fromJust $
                fromString "b1080b06-e9cc-11e6-bf0e-fe55135034f3")))
            -- eid
            -- a dwhat element
            (
              ObjectDWhat Observe
              [
                IL $ SGTIN "0614141" Nothing "107346" "2017",
                IL $ SGTIN "0614141" Nothing "107346" "2018",
                CL (LGTIN "4012345" "012345" "998877")
                    (Just $ MeasuredQuantity 200 "KGM")
              ]
            )
            -- a dwhen element
            (
              DWhen
                (read "2005-04-03 20:33:31.116-06:00" :: UTCTime)
                Nothing
                (read "-06:00" :: TimeZone)
            )
            -- a dwhy element
            (DWhy (Just Shipping) (Just InTransit))
            -- a dwhere element
            (
              DWhere
              [SGLN "0614141" (LocationReferenceNum "00777") Nothing]
              -- [ReadPointLocation]
              [SGLN "0614141" (LocationReferenceNum "00888") Nothing]
              -- [BizLocation]
              [
                (
                  SDPossessingParty, -- SourceDestType
                  SGLN "4012345" (LocationReferenceNum "00001") Nothing
                  -- LocationEPC
                )
              ] -- srcType
              [
                (
                  SDOwningParty,
                  SGLN "0614141" (LocationReferenceNum "00001") Nothing
                ),
                (
                  SDLocation,
                  SGLN "0614141" (LocationReferenceNum "00777") Nothing
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
            -- @todo annonate the attributes with comments about what they are
              ObjectEventT -- type
              -- eid
              (Just (EventID (fromJust $
                  fromString "b1080840-e9cc-11e6-bf0e-fe55135034f3")))
              -- a dwhat element
              (
                ObjectDWhat Observe
                [
                  IL $ SGTIN "0614141" Nothing "107346" "2017",
                  IL $ SGTIN "0614141" Nothing "107346" "2018"
                ]
              )
              -- a dwhen element
              (
                DWhen
                  (read "2005-04-03 20:33:31.116-06:00" :: UTCTime)
                  (Just (read "2005-04-03 20:33:31.116-06:00" :: UTCTime))
                  (read "-06:00" :: TimeZone)
              )
              -- a dwhy element
              (DWhy (Just Shipping) (Just InTransit))
              -- a dwhere element
              -- <id>urn:epc:id:sgln:0614141.07346.1234</id>
              (
                DWhere
                  [SGLN "0614141" (LocationReferenceNum "07346") (Just "1234")]
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
                ObjectDWhat Observe
                  [IL $ SGTIN "0614141" Nothing "107346" "2018"]
              )
              -- a dwhen element
              (
                DWhen
                  (read "2005-04-04 20:33:31.116-06:00" :: UTCTime)
                  Nothing
                  (read "-06:00" :: TimeZone)
              )
              -- a dwhy element
              (DWhy (Just Receiving) (Just InProgress))
              -- a dwhere element
              (
                DWhere
                [SGLN "0012345" (LocationReferenceNum "11111") (Just "400")]
                -- [ReadPointLocation]
                [SGLN "0012345" (LocationReferenceNum "11111") Nothing]
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
          -- @todo annonate the attributes with comments about what they are
          TransformationEventT -- type
          Nothing -- eid
          -- a dwhat element
          (
            TransformationDWhat Nothing
              [
                IL (SGTIN "4012345" Nothing "011122" "25"),
                IL (SGTIN "4000001" Nothing "065432" "99886655"),
                CL (LGTIN "4012345" "011111" "4444")
                    (Just (MeasuredQuantity 10.0 "KGM")),
                CL (LGTIN "0614141" "077777" "987") (Just (ItemCount 30)),
                CL (CSGTIN "4012345" Nothing "066666") (Just (ItemCount 220))
              ] 
              [
                IL (SGTIN "4012345" Nothing "077889" "25"),
                IL (SGTIN "4012345" Nothing "077889" "26"),
                IL (SGTIN "4012345" Nothing "077889" "27"),
                IL (SGTIN "4012345" Nothing "077889" "28")
              ]
          )
          -- a dwhen element
          (
            DWhen
              (read "2013-10-31 14:58:56.591Z" :: UTCTime) -- eventTime
              Nothing -- recordTime
              (read "+02:00" :: TimeZone) -- timeZone
          )
          -- a dwhy element
          (DWhy (Just Commissioning) (Just InProgress))
          -- a dwhere element
          (
            DWhere
            [SGLN "4012345" (LocationReferenceNum "00001") Nothing]
            -- [ReadPointLocation]
            [] -- [BizLocation]
            [] -- srcType
            [] -- destType
          )
        ]

    -- @todo change this!    
    it "parses a valid transaction event" $ do -- @matt
      doc <- Text.XML.readFile def "test/test-xml/TransactionEvent.xml"
      let cursor = fromDocument doc
      let parsedEvents = parseEventByType cursor TransactionEventT
      -- this is a template to write tests for the events
      parsedEvents`shouldBe`
            -- a huge dwhat element
        [Right $ Event
          -- @todo annonate the attributes with comments about what they are
          TransactionEventT -- type
          (Just (EventID (fromJust $
              fromString "b1080840-e9cc-11e6-bf0e-fe55135034f3")))
          -- eid
          -- a dwhat element
          (
            TransactionDWhat Observe
            Nothing
            [
              BizTransaction{_btid="http://transaction.acme.com/po/12345678", _bt=Po}
            ]
            [
              IL $ SGTIN "0614141" Nothing "107346" "2017",
              IL $ SGTIN "0614141" Nothing "107346" "2018"
            ]
          )
          -- a dwhen element
          (
            DWhen
              (read "2005-04-03 20:33:31.116-06:00" :: UTCTime)
              (Just (read "2005-04-03 20:33:31.116-06:00" :: UTCTime))
              (read "-06:00" :: TimeZone)
          )
          -- a dwhy element
          (DWhy (Just Shipping) (Just InTransit))
          -- a dwhere element
          (
            DWhere
            [SGLN "0614141" (LocationReferenceNum "07346") (Just "1234")]
            -- [ReadPointLocation]
            []-- [BizLocation]
            [] -- srcType
            [] -- destType
          ),

          -- second element
          Right $ Event
          -- @todo annonate the attributes with comments about what they are
          TransactionEventT -- type
          (Just (EventID (fromJust $
              fromString "b108094e-e9cc-11e6-bf0e-fe55135034f3")))
          -- eid
          -- a dwhat element
          (
            TransactionDWhat Observe
            Nothing
            [
              BizTransaction{_btid="http://transaction.acme.com/po/12345678", _bt=Po},
              BizTransaction{_btid="urn:epcglobal:cbv:bt:0614141073467:1152", _bt=Desadv}
            ]
            [IL $ SGTIN "0614141" Nothing "107346" "2018"]
          )
          -- a dwhen element
          (
            DWhen
              (read "2005-04-04 20:33:31.116-06:00" :: UTCTime)
              Nothing
              (read "-06:00" :: TimeZone)
          )
          -- a dwhy element
          (DWhy (Just Receiving) (Just InProgress))
          -- a dwhere element
          (
            DWhere
            [SGLN "0012345" (LocationReferenceNum "11111") (Just "400")]
            -- [ReadPointLocation]
            [SGLN "0012345" (LocationReferenceNum "11111") Nothing]
            -- [BizLocation]
            [] -- srcType
            [] -- destType
          )
        ]
    
    -- @todo change this!
    it "parses a valid aggregation event" $ do -- @matt
      doc <- Text.XML.readFile def "test/test-xml/AggregationEvent.xml"
      let cursor = fromDocument doc
      let parsedEvents = parseEventByType cursor AggregationEventT
      -- this is a template to write tests for the events
      parsedEvents`shouldBe`
            -- a huge dwhat element
        [Right $ Event
          -- @todo annonate the attributes with comments about what they are
          AggregationEventT -- type
          (Just (EventID (fromJust $
              fromString "b1080840-e9cc-11e6-bf0e-fe55240134d5")))
          -- eid
          -- a dwhat element
          (
            AggregationDWhat Observe
            -- <parentID>urn:epc:id:sscc:0614141.1234567890</parentID>
                                  -- | SSCC GS1CompanyPrefix SerialNumber

            (Just (SSCC "0614141" "1234567890")) -- Maybe ParentID
            [
              IL $ SGTIN "0614141" Nothing "107346" "2017",
              IL $ SGTIN "0614141" Nothing "107346" "2018",
              CL (CSGTIN "4012345" Nothing "098765") (Just $ ItemCount 10),
              CL (LGTIN "4012345" "012345" "998877")
                  (Just $ MeasuredQuantity 200.5 "KGM")
            ] -- [LabelEPC]
          )
          -- a dwhen element
          (
            DWhen
              (read "2013-06-08 14:58:56.591+02:00" :: UTCTime)
              Nothing
              (read "+02:00" :: TimeZone)
          )
          -- a dwhy element
          (DWhy (Just Receiving) (Just InProgress))
          -- a dwhere element
          (
            DWhere
            [SGLN "0614141" (LocationReferenceNum "00777") Nothing]
            -- [ReadPointLocation]
            [SGLN "0614141" (LocationReferenceNum "00888") Nothing]
            -- [BizLocation]
            [] -- srcType
            [] -- destType
          )
        ]


  describe "test some basic functions in Parser" $ do
    it "parseSingleElemE invalid" $
      parseSingleElemE Right [] `shouldBe` Left TagNotFound
    it "parseSingleElemE valid" $
      parseSingleElemE Right ["hi"] `shouldBe` Right "hi"

    it "parseSingleElemM invalid" $
      parseSingleElemM Just [] `shouldBe` Nothing
    it "parseSingleElemM invalid 2" $
      parseSingleElemM (const (Nothing :: Maybe String)) ["hi"] `shouldBe` Nothing
    it "parseSingleElemM valid" $
      parseSingleElemM Just ["hi"] `shouldBe` Just "hi"

    it "parseTimeXML invalid" $
      parseTimeXML [] `shouldBe` Left TagNotFound
    it "parseTimeXML invalid 2" $
      parseTimeXML
        ["the quick brown fox jumped over the lazy dog", "2005-04-03T20:33:31.116-06:00"]
          `shouldBe` Left TimeZoneError
    it "parseTimeXML valid" $
      parseTimeXML ["2005-04-03T20:33:31.116-06:00", "the quick brown fox jumped over the lazy dog"] `shouldBe` Right 
         (read "2005-04-03 20:33:31.116-06:00" :: UTCTime)

    it "parseTimeZoneXML invalid" $
      parseTimeZoneXML [] `shouldBe` Left TagNotFound
    it "parseTimeZoneXML invalid 2" $
      parseTimeZoneXML ["the quick brown fox jumped over the lazy dog", "2005-04-03T20:33:31.116-06:00"] `shouldBe` Left TimeZoneError
    it "parseTimeZoneXML valid" $
      parseTimeZoneXML
        ["-06:00", "the quick brown fox jumped over the lazy dog"]
          `shouldBe`
            Right (read "-06:00" :: TimeZone)
