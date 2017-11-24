module Tests.DWhere where

import           Data.GS1.EPC
import           Data.GS1.DWhere
import           Data.GS1.URI
import           Test.Hspec

type EitherLE = Either LocationError LocationEPC
--                                  Experimental
testPassGLN :: Spec
testPassGLN =
  describe "Location" $ do
    it "GLN is verified correctly" $
      (gln "0614141" "18133" "9" :: EitherLE) `shouldBe` Right (GLN "0614141" "18133" "9")

    it "GLN is verified correctly" $
      (gln "0532132" "14112" "7" :: EitherLE) `shouldBe` Right (GLN "0532132" "14112" "7")

    it "GLN is verified correctly from 20141216 onesteel" $
      (gln "9348585" "01002" "3" :: EitherLE) `shouldBe` Right (GLN "9348585" "01002" "3")

    -- the GLN is incorrect on the spreadsheet as it contains 12 digits only
    it "GLN is verified correctly from 20141216 onesteel" $
      (gln "9348585" "08001" "9" :: EitherLE) `shouldBe` Right (GLN "9348585" "08001" "9")

    it "IllegalFormat: Invalid length" $
      (gln "0614141" "1813392322222222222" "2" :: EitherLE) `shouldBe` Left IllegalGLNFormat

    it "IllegalFormat: Invalid length" $
      (gln "" "" "" :: EitherLE) `shouldBe` Left IllegalGLNFormat

    it "IllegalFormat: Invalid character" $
      (gln "0614141" "181ab" "9" :: EitherLE) `shouldBe` Left IllegalGLNFormat

    it "InvalidChecksum"  $
      (gln "0614141" "18133" "5" :: EitherLE) `shouldBe` Left InvalidChecksum

    it "PrettyPrint Location as URI" $
      ppURI (Location (GLN "0614141" "18133" "9")) `shouldBe` "urn:epc:id:sgln:0614141.18133.9"

    it "Returns the proper URI prefix" $
      uriPrefix (Location (GLN "0614141" "18133" "9")) `shouldBe` "urn:epc:id"

    it "Returns the proper URI quantifier" $
      uriQuantifier (Location (GLN "0614141" "18133" "9")) `shouldBe` "sgln"

    it "Returns the proper URI quantifier" $
      uriPayload (Location (GLN "0614141" "18133" "9")) `shouldBe` "0614141.18133.9"

testMkEPC :: Spec
testMkEPC =
  describe "EPC" $ do
    it "make a GLN from valid string" $
      mkEPC "GLN" "0532132.14112.7" `shouldBe` Just (GLN "0532132" "14112" "7")

    it "make a GLN from valid string" $
      mkEPC "GLN" "0614141.18133.9" `shouldBe` Just (GLN "0614141" "18133" "9")

    it "make Nothing from invalid string" $
      mkEPC "GLN" "0614141.18133.5" `shouldBe` Nothing

    it "make Nothing from invalid string" $
      mkEPC "GLN" "0614141.18133.5121" `shouldBe` Nothing
    
    it "make Nothing from invalid string" $
      mkEPC "GLN" "really bad input" `shouldBe` Nothing
