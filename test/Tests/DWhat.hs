{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings     #-}

module Tests.DWhat where

-- import           Data.Maybe
import           Test.Hspec
import qualified Data.Text as T
import           Data.GS1.DWhat
import           Data.GS1.EPC

testLabelEPC :: Spec
testLabelEPC = do
  describe "Agnostic readLabelEPC -> urn2LabelEPC" $ do
    -- since SGTIN can be both Instance and Class
    describe "SGTINs" $ do
      it "Instance SGTIN" $
        urn2LabelEPC "urn:epc:id:sgtin:0614141.107346.2017"
          `shouldBe`
            (Right $ IL $ SGTIN "0614141" Nothing "107346" "2017")
      it "Class SGTIN" $
        urn2LabelEPC "urn:epc:idpat:sgtin:4012345.098765.*"
          `shouldBe`
            (Right $ CL (CSGTIN "4012345" Nothing "098765") Nothing)
    describe "Non SGTINs" $ do
      it "Instance SSCC" $
        urn2LabelEPC "urn:epc:id:sscc:0614141.1234567890"
          `shouldBe`
            (Right $ IL $ SSCC "0614141" "1234567890")
      it "Class LGTIN" $
        urn2LabelEPC "urn:epc:class:lgtin:4012345.012345.998877"
          `shouldBe`
            (Right $ CL (LGTIN "4012345" "012345" "998877") Nothing)
testBizStep :: Spec
testBizStep = do
  describe "BusinessStep" $ do
    it "produces correct URI" $
      printURI Accepting `shouldBe` "urn:epcglobal:cbv:bizstep:accepting"
    it "produces correct URI" $
      printURI CycleCounting `shouldBe`
        "urn:epcglobal:cbv:bizstep:cycle_counting"
    it "produces correct URI" $
      printURI CreatingClassInstance `shouldBe`
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
      printURI Bol `shouldBe` "urn:epcglobal:cbv:btt:bol"
    it "print BizTransaction 2" $
      printURI Prodorder `shouldBe` "urn:epcglobal:cbv:btt:prodorder"

testPpDWhat :: Spec
testPpDWhat = do
  describe "create valid ObjectDWhat" $ do
    it "creates ObjectDWhat from valid input, Observe" $
      ppDWhat (ObjectDWhat Observe
        [IL (SGTIN "0614141" Nothing "107346" "2017"),
        IL (SGTIN "0614141" Nothing "107346" "2018")])
          `shouldBe`
            "OBJECT WHAT\nObserve\n" ++
            "[IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = \"107346\", _sgtinSerialNum = \"2017\"}}," ++
            "IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = \"107346\", _sgtinSerialNum = \"2018\"}}]\n"

    it "creates ObjectDWhat from valid input, Add" $
      ppDWhat (ObjectDWhat Add [IL (SGTIN "0614141" Nothing "107346" "2017"),
        IL (SGTIN "0614141" Nothing "107346" "2018")])
          `shouldBe`
            "OBJECT WHAT\nAdd\n" ++
            "[IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = \"107346\", _sgtinSerialNum = \"2017\"}}" ++
            ",IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = \"107346\", _sgtinSerialNum = \"2018\"}}]\n"
    it "creates ObjectDWhat from valid input, Delete" $
      ppDWhat (ObjectDWhat Delete [IL (SGTIN "0614141" Nothing "107346" "2017"),
        IL (SGTIN "0614141" Nothing "107346" "2018")])
          `shouldBe`
            "OBJECT WHAT\nDelete\n" ++
            "[IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = \"107346\", _sgtinSerialNum = \"2017\"}}," ++
            "IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = \"107346\", _sgtinSerialNum = \"2018\"}}]\n"

  describe "create from empty epcs" $ do
    it "creates DWhat from empty epc list, Add" $
      ppDWhat (ObjectDWhat Add []) `shouldBe` "OBJECT WHAT\nAdd\n[]\n"
    it "creates DWhat from empty epc list, Delete" $
      ppDWhat (ObjectDWhat Delete []) `shouldBe` "OBJECT WHAT\nDelete\n[]\n"
    it "creates DWhat from empty epc list Observe" $
      ppDWhat (ObjectDWhat Observe []) `shouldBe` "OBJECT WHAT\nObserve\n[]\n"

  describe "create valid AggregationDWhat" $ do
    it "creates AggregationDWhat from valid input, Observe" $
      ppDWhat (AggregationDWhat Observe
        (Just (SSCC "0614141" "1234567890"))
        [IL (SGTIN "0614141" Nothing "107346" "2017"),
        IL (SGTIN "0614141" Nothing "107346" "2018")])
          `shouldBe`
            "AGGREGATION WHAT\nObserve\n" ++
            "Just (SSCC {_ssccCompanyPrefix = \"0614141\", _ssccSerialNum = \"1234567890\"})\n" ++
            "[IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = \"107346\", _sgtinSerialNum = \"2017\"}}," ++
            "IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = \"107346\", _sgtinSerialNum = \"2018\"}}]\n"

    it "creates AggregationDWhat from valid input, Add" $
      ppDWhat (AggregationDWhat Add
        (Just (SSCC "0614141" "1234567890"))
        [IL (SGTIN "0614141" Nothing "107346" "2017"),
        IL (SGTIN "0614141" Nothing "107346" "2018")])
          `shouldBe`
            "AGGREGATION WHAT\nAdd\n" ++
            "Just (SSCC {_ssccCompanyPrefix = \"0614141\", _ssccSerialNum = \"1234567890\"})\n" ++
            "[IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = \"107346\", _sgtinSerialNum = \"2017\"}}," ++
            "IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = \"107346\", _sgtinSerialNum = \"2018\"}}]\n"

    it "creates AggregationDWhat from valid input, Delete" $
      ppDWhat (AggregationDWhat Delete
        (Just (SSCC "0614141" "1234567890"))
        [IL (SGTIN "0614141" Nothing "107346" "2017"),
        IL (SGTIN "0614141" Nothing "107346" "2018")])
          `shouldBe`
            "AGGREGATION WHAT\nDelete\n" ++
            "Just (SSCC {_ssccCompanyPrefix = \"0614141\", _ssccSerialNum = \"1234567890\"})\n" ++
            "[IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = \"107346\", _sgtinSerialNum = \"2017\"}}," ++
            "IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = \"107346\", _sgtinSerialNum = \"2018\"}}]\n"

    it "createAggregationDWhat from empty, Delete" $
      ppDWhat (AggregationDWhat Delete Nothing [])
        `shouldBe` "AGGREGATION WHAT\nDelete\nNothing\n[]\n"
    it "createAggregationDWhat from empty, Add" $
      ppDWhat (AggregationDWhat Add Nothing [])
        `shouldBe` "AGGREGATION WHAT\nAdd\nNothing\n[]\n"
    it "createAggregationDWhat from empty, Observe" $
      ppDWhat (AggregationDWhat Observe Nothing [])
        `shouldBe` "AGGREGATION WHAT\nObserve\nNothing\n[]\n"


  describe "work with a TransactionDWhat" $ do
    it "create TransactionDWhat from valid input, Add" $
      ppDWhat (TransactionDWhat Add
        (Just (SSCC "0614141" "1234567890"))
        [BizTransaction{_btid="12345", _bt=Bol}]
        [IL (SGTIN "0614141" Nothing "107346" "2017"),
        IL (SGTIN "0614141" Nothing "107346" "2018")])
          `shouldBe`
            "TRANSACTION WHAT\nAdd\n" ++
            "Just (SSCC {_ssccCompanyPrefix = \"0614141\", _ssccSerialNum = \"1234567890\"})\n" ++
            "[BizTransaction {_btid = \"12345\", _bt = Bol}]\n" ++
            "[IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = \"107346\", _sgtinSerialNum = \"2017\"}}," ++
            "IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = \"107346\", _sgtinSerialNum = \"2018\"}}]\n"

    it "create TransactionDWhat from valid input, Observe" $
      ppDWhat (TransactionDWhat Observe
        (Just (SSCC "0614141" "1234567890"))
        [BizTransaction{_btid="12345", _bt=Bol}]
        [IL (SGTIN "0614141" Nothing "107346" "2017"),
        IL (SGTIN "0614141" Nothing "107346" "2018")])
          `shouldBe`
            "TRANSACTION WHAT\nObserve\n" ++
            "Just (SSCC {_ssccCompanyPrefix = \"0614141\", _ssccSerialNum = \"1234567890\"})\n" ++
            "[BizTransaction {_btid = \"12345\", _bt = Bol}]\n" ++
            "[IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = \"107346\", _sgtinSerialNum = \"2017\"}}," ++
            "IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = \"107346\", _sgtinSerialNum = \"2018\"}}]\n"

    it "create TransactionDWhat from valid input, Delete" $
      ppDWhat (TransactionDWhat Delete
        (Just (SSCC "0614141" "1234567890"))
        [BizTransaction{_btid="12345", _bt=Bol}]
        [IL (SGTIN "0614141" Nothing "107346" "2017"),
        IL (SGTIN "0614141" Nothing "107346" "2018")])
          `shouldBe`
            "TRANSACTION WHAT\nDelete\n" ++
            "Just (SSCC {_ssccCompanyPrefix = \"0614141\", _ssccSerialNum = \"1234567890\"})\n" ++
            "[BizTransaction {_btid = \"12345\", _bt = Bol}]\n" ++
            "[IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = \"107346\", _sgtinSerialNum = \"2017\"}}," ++
            "IL {_ilInstanceLabelEpc = SGTIN {_sgtinCompanyPrefix = \"0614141\", _sgtinSgtinFilterValue = Nothing, _sgtinItemReference = \"107346\", _sgtinSerialNum = \"2018\"}}]\n"

    it "empty TransactionDWhat with empty, Add" $
      ppDWhat (TransactionDWhat Add Nothing [] [])
        `shouldBe` "TRANSACTION WHAT\nAdd\nNothing\n[]\n[]\n"
    it "empty TransactionDWhat with empty, Delete" $
      ppDWhat (TransactionDWhat Delete Nothing [] [])
        `shouldBe` "TRANSACTION WHAT\nDelete\nNothing\n[]\n[]\n"
    it "empty TransactionDWhat with empty, Observe" $
      ppDWhat (TransactionDWhat Observe Nothing [] [])
        `shouldBe` "TRANSACTION WHAT\nObserve\nNothing\n[]\n[]\n"


  describe "work with a TransformationDWhat" $ do
    it "create TransformationDWhat, empty" $
      ppDWhat (TransformationDWhat Nothing [] [])
        `shouldBe` "TRANSFORMATION WHAT\nNothing\n[]\n[]\n"
