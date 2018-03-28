{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.DWhat where

-- import           Data.Maybe
import           Data.GS1.DWhat
import           Data.GS1.EPC
import qualified Data.Text      as T
import           Test.Hspec

testLabelEPC :: Spec
testLabelEPC =
  describe "Agnostic readLabelEPC -> urn2LabelEPC" $ do
    -- since SGTIN can be both Instance and Class
    describe "SGTINs" $ do
      it "Instance SGTIN" $
        urn2LabelEPC "urn:epc:id:sgtin:0614141.107346.2017"
          `shouldBe`
            (Right $ IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2017"))
      it "Class SGTIN" $
        urn2LabelEPC "urn:epc:idpat:sgtin:4012345.098765.*"
          `shouldBe`
            (Right $ CL (CSGTIN (GS1CompanyPrefix "4012345") Nothing (ItemReference "098765")) Nothing)
    describe "Non SGTINs" $ do
      it "Instance SSCC" $
        urn2LabelEPC "urn:epc:id:sscc:0614141.1234567890"
          `shouldBe`
            (Right $ IL $ SSCC (GS1CompanyPrefix "0614141") (SerialNumber "1234567890"))
      it "Class LGTIN" $
        urn2LabelEPC "urn:epc:class:lgtin:4012345.012345.998877"
          `shouldBe`
            (Right $ CL (LGTIN (GS1CompanyPrefix "4012345") (ItemReference "012345") (Lot "998877")) Nothing)
testBizStep :: Spec
testBizStep = do
  describe "BusinessStep" $ do
    it "produces correct URI" $
      renderURL Accepting `shouldBe` "urn:epcglobal:cbv:bizstep:accepting"
    it "produces correct URI" $
      renderURL CycleCounting `shouldBe`
        "urn:epcglobal:cbv:bizstep:cycle_counting"
    it "produces correct URI" $
      renderURL CreatingClassInstance `shouldBe`
        "urn:epcglobal:cbv:bizstep:creating_class_instance"

  describe "parseBizStep" $ do
    it "parse valid uri to bizstep" $
      readURI "urn:epcglobal:cbv:bizstep:void_shipping" `shouldBe`
        Right VoidShipping

    it "parse valid uri to bizstep" $
      readURI "urn:epcglobal:cbv:bizstep:accepting" `shouldBe` Right Accepting

  describe "Invalid urns" $ do
    it "parse valid uri but invalid step to Nothing" $
      (readURI :: T.Text -> Either ParseFailure BizStep)
        "urn:epcglobal:cbv:bizstep:s"
          `shouldBe` Left InvalidFormat

    it "parse invalid uri to Nothing" $
      (readURI :: T.Text -> Either ParseFailure BizStep)
        "urn:invalidns:cbv:bizstep:void_shipping"
          `shouldBe` Left InvalidFormat
    it "A component of the urn missing" $
      (readURI :: T.Text -> Either ParseFailure BizStep) "urn:epc:cbv:bizstep"
        `shouldBe` Left InvalidFormat
    it "empty" $
      (readURI :: T.Text -> Either ParseFailure BizStep) ""
        `shouldBe` Left InvalidFormat

testBizTransaction :: Spec
testBizTransaction = do
  describe "Parse BizTransactionID" $
    it "parse the valid uri to BizTransactionID" $
      readURI "urn:epcglobal:cbv:btt:po" `shouldBe` Right Po

  describe "Invalid urns" $ do
    it "parse the invalid uri to Nothing" $
      (readURI :: T.Text -> Either ParseFailure BizTransactionType)
        "urn:epcglobal:cbv:btt:somethingelse"
          `shouldBe` Left InvalidFormat
    it "parse the empty uri to Nothing" $
      (readURI :: T.Text -> Either ParseFailure BizTransactionType)
        "" `shouldBe` Left InvalidFormat
    it "parse a uri missing component with to Nothing" $
      (readURI :: T.Text -> Either ParseFailure BizTransactionType)
        "urn:epcglobal:cbv:po"
          `shouldBe` Left InvalidFormat
    it "parse a rubbish uri with no : to Nothing" $
      (readURI :: T.Text -> Either ParseFailure BizTransactionType)
        "fooblahjaja" `shouldBe` Left InvalidFormat

  describe "print BizTransaction" $ do
    it "print BizTransaction 1" $
      renderURL Bol `shouldBe` "urn:epcglobal:cbv:btt:bol"
    it "print BizTransaction 2" $
      renderURL Prodorder `shouldBe` "urn:epcglobal:cbv:btt:prodorder"

testPpDWhat :: Spec
testPpDWhat = do
  describe "create valid ObjectDWhat" $ do
    it "creates ObjectDWhat from valid input, Observe" $
      ppDWhat (ObjWhat $ ObjectDWhat Observe
        [IL (SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2017")),
        IL (SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018"))])
          `shouldBe`
          unlines
          ["OBJECT WHAT"
          ,"Observe"
          ,"[IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = GS1CompanyPrefix \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = ItemReference \"107346\", _sgtinSerialNum = SerialNumber \"2017\"}}"
            ++",IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = GS1CompanyPrefix \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = ItemReference \"107346\", _sgtinSerialNum = SerialNumber \"2018\"}}]"
          ]

    it "creates ObjectDWhat from valid input, Add" $
      ppDWhat (ObjWhat $ ObjectDWhat Add [IL (SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2017")),
        IL (SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018"))])
          `shouldBe`
          unlines
          ["OBJECT WHAT"
          ,"Add"
          ,"[IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = GS1CompanyPrefix \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = ItemReference \"107346\", _sgtinSerialNum = SerialNumber \"2017\"}}"
            ++",IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = GS1CompanyPrefix \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = ItemReference \"107346\", _sgtinSerialNum = SerialNumber \"2018\"}}]"
          ]
    it "creates ObjectDWhat from valid input, Delete" $
      ppDWhat (ObjWhat $ ObjectDWhat Delete [IL (SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2017")),
        IL (SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018"))])
          `shouldBe`
          unlines
          ["OBJECT WHAT"
          ,"Delete"
          ,"[IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = GS1CompanyPrefix \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = ItemReference \"107346\", _sgtinSerialNum = SerialNumber \"2017\"}}"
            ++",IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = GS1CompanyPrefix \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = ItemReference \"107346\", _sgtinSerialNum = SerialNumber \"2018\"}}]"
          ]

  describe "create from empty epcs" $ do
    it "creates DWhat from empty epc list, Add" $
      ppDWhat (ObjWhat $ ObjectDWhat Add []) `shouldBe` "OBJECT WHAT\nAdd\n[]\n"
    it "creates DWhat from empty epc list, Delete" $
      ppDWhat (ObjWhat $ ObjectDWhat Delete []) `shouldBe` "OBJECT WHAT\nDelete\n[]\n"
    it "creates DWhat from empty epc list Observe" $
      ppDWhat (ObjWhat $ ObjectDWhat Observe []) `shouldBe` "OBJECT WHAT\nObserve\n[]\n"

  describe "create valid AggregationDWhat" $ do
    it "creates AggregationDWhat from valid input, Observe" $
      ppDWhat (AggWhat $ AggregationDWhat Observe
        (Just (ParentLabel $ SSCC (GS1CompanyPrefix "0614141") (SerialNumber "1234567890")))
        [IL (SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2017")),
        IL (SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018"))])
          `shouldBe`
          unlines
            ["AGGREGATION WHAT"
            ,"Observe"
            ,"Just (ParentLabel (SSCC {_ssccCompanyPrefix = GS1CompanyPrefix \"0614141\", _ssccSerialNum = SerialNumber \"1234567890\"}))"
            ,"[IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = GS1CompanyPrefix \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = ItemReference \"107346\", _sgtinSerialNum = SerialNumber \"2017\"}}"
              ++",IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = GS1CompanyPrefix \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = ItemReference \"107346\", _sgtinSerialNum = SerialNumber \"2018\"}}]"
            ]

    it "creates AggregationDWhat from valid input, Add" $
      ppDWhat (AggWhat $ AggregationDWhat Add
        (Just (ParentLabel $ SSCC (GS1CompanyPrefix "0614141") (SerialNumber "1234567890")))
        [IL (SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2017")),
        IL (SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018"))])
          `shouldBe`
          unlines
            ["AGGREGATION WHAT"
            ,"Add"
            ,"Just (ParentLabel (SSCC {_ssccCompanyPrefix = GS1CompanyPrefix \"0614141\", _ssccSerialNum = SerialNumber \"1234567890\"}))"
            ,"[IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = GS1CompanyPrefix \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = ItemReference \"107346\", _sgtinSerialNum = SerialNumber \"2017\"}}"
              ++",IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = GS1CompanyPrefix \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = ItemReference \"107346\", _sgtinSerialNum = SerialNumber \"2018\"}}]"
            ]

    it "creates AggregationDWhat from valid input, Delete" $
      ppDWhat (AggWhat $ AggregationDWhat Delete
        (Just (ParentLabel $ SSCC (GS1CompanyPrefix "0614141") (SerialNumber "1234567890")))
        [IL (SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2017")),
        IL (SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018"))])
          `shouldBe`
          unlines
            ["AGGREGATION WHAT"
            ,"Delete"
            ,"Just (ParentLabel (SSCC {_ssccCompanyPrefix = GS1CompanyPrefix \"0614141\", _ssccSerialNum = SerialNumber \"1234567890\"}))"
            ,"[IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = GS1CompanyPrefix \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = ItemReference \"107346\", _sgtinSerialNum = SerialNumber \"2017\"}}"
              ++",IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = GS1CompanyPrefix \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = ItemReference \"107346\", _sgtinSerialNum = SerialNumber \"2018\"}}]"
            ]

    it "createAggregationDWhat from empty, Delete" $
      ppDWhat (AggWhat $ AggregationDWhat Delete Nothing [])
        `shouldBe` "AGGREGATION WHAT\nDelete\nNothing\n[]\n"
    it "createAggregationDWhat from empty, Add" $
      ppDWhat (AggWhat $ AggregationDWhat Add Nothing [])
        `shouldBe` "AGGREGATION WHAT\nAdd\nNothing\n[]\n"
    it "createAggregationDWhat from empty, Observe" $
      ppDWhat (AggWhat $ AggregationDWhat Observe Nothing [])
        `shouldBe` "AGGREGATION WHAT\nObserve\nNothing\n[]\n"


  describe "work with a TransactionDWhat" $ do
    it "create TransactionDWhat from valid input, Add" $
      ppDWhat (TransactWhat $ TransactionDWhat Add
        (Just (ParentLabel $ SSCC (GS1CompanyPrefix "0614141") (SerialNumber "1234567890")))
        [BizTransaction{_btid=BizTransactionID "12345", _bt=Bol}]
        [IL (SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2017")),
        IL (SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018"))])
          `shouldBe`
            unlines
              ["TRANSACTION WHAT"
              ,"Add"
              ,"Just (ParentLabel (SSCC {_ssccCompanyPrefix = GS1CompanyPrefix \"0614141\", _ssccSerialNum = SerialNumber \"1234567890\"}))"
              ,"[BizTransaction {_btid = BizTransactionID \"12345\", _bt = Bol}]"
              ,"[IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = GS1CompanyPrefix \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = ItemReference \"107346\", _sgtinSerialNum = SerialNumber \"2017\"}}"
                ++",IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = GS1CompanyPrefix \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = ItemReference \"107346\", _sgtinSerialNum = SerialNumber \"2018\"}}]"
              ]

    it "create TransactionDWhat from valid input, Observe" $
      ppDWhat (TransactWhat $ TransactionDWhat Observe
        (Just (ParentLabel $ SSCC (GS1CompanyPrefix "0614141") (SerialNumber "1234567890")))
        [BizTransaction{_btid= BizTransactionID "12345", _bt=Bol}]
        [IL (SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2017")),
        IL (SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018"))])
          `shouldBe`
            unlines
              ["TRANSACTION WHAT"
              ,"Observe"
              ,"Just (ParentLabel (SSCC {_ssccCompanyPrefix = GS1CompanyPrefix \"0614141\", _ssccSerialNum = SerialNumber \"1234567890\"}))"
              ,"[BizTransaction {_btid = BizTransactionID \"12345\", _bt = Bol}]"
              ,"[IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = GS1CompanyPrefix \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = ItemReference \"107346\", _sgtinSerialNum = SerialNumber \"2017\"}}"
                ++",IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = GS1CompanyPrefix \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = ItemReference \"107346\", _sgtinSerialNum = SerialNumber \"2018\"}}]"
              ]

    it "create TransactionDWhat from valid input, Delete" $
      ppDWhat (TransactWhat $ TransactionDWhat Delete
        (Just (ParentLabel $ SSCC (GS1CompanyPrefix "0614141") (SerialNumber "1234567890")))
        [BizTransaction{_btid= BizTransactionID "12345", _bt=Bol}]
        [IL (SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2017")),
        IL (SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018"))])
          `shouldBe`
            unlines
              ["TRANSACTION WHAT"
              ,"Delete"
              ,"Just (ParentLabel (SSCC {_ssccCompanyPrefix = GS1CompanyPrefix \"0614141\", _ssccSerialNum = SerialNumber \"1234567890\"}))"
              ,"[BizTransaction {_btid = BizTransactionID \"12345\", _bt = Bol}]"
              ,"[IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = GS1CompanyPrefix \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = ItemReference \"107346\", _sgtinSerialNum = SerialNumber \"2017\"}}"
                ++",IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = GS1CompanyPrefix \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = ItemReference \"107346\", _sgtinSerialNum = SerialNumber \"2018\"}}]"
              ]

    it "empty TransactionDWhat with empty, Add" $
      ppDWhat (TransactWhat $ TransactionDWhat Add Nothing [] [])
        `shouldBe` "TRANSACTION WHAT\nAdd\nNothing\n[]\n[]\n"
    it "empty TransactionDWhat with empty, Delete" $
      ppDWhat (TransactWhat $ TransactionDWhat Delete Nothing [] [])
        `shouldBe` "TRANSACTION WHAT\nDelete\nNothing\n[]\n[]\n"
    it "empty TransactionDWhat with empty, Observe" $
      ppDWhat (TransactWhat $ TransactionDWhat Observe Nothing [] [])
        `shouldBe` "TRANSACTION WHAT\nObserve\nNothing\n[]\n[]\n"


  describe "work with a TransformationDWhat" $ do
    it "create TransformationDWhat, empty" $
      ppDWhat (TransformWhat $ TransformationDWhat Nothing [] [])
        `shouldBe` "TRANSFORMATION WHAT\nNothing\n[]\n[]\n"

    it "create TransformationDWhat, empty" $
      ppDWhat
        (TransformWhat $ TransformationDWhat Nothing
          [IL (SGTIN "0614141" Nothing "107346" "2017"),
          IL (SGTIN "0614141" Nothing "107346" "2018")]

          [CL (CSGTIN "4012345" Nothing "098765") Nothing]
          )
        `shouldBe`
          "TRANSFORMATION WHAT\nNothing\n" ++
          "[IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = \"107346\", _sgtinSerialNum = \"2017\"}}," ++
          "IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = \"107346\", _sgtinSerialNum = \"2018\"}}]\n" ++
          "[CL {_clClassLabelEpc = CSGTIN {_csgtinCompanyPrefix = \"4012345\", _csgtinSgtinFilterValue = Nothing, _csgtinItemReference = \"098765\"}, " ++
          "_clQuantity = Nothing}]\n"
