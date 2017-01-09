module Tests.Location where

import           Data.GS1.EPC
import           Data.GS1.Location
import           Data.GS1.URI
import           Test.Hspec

type EitherLE = Either LocationError EPC

testPassGLN :: Spec
testPassGLN =
  describe "Location" $ do
    it "GLN is verified correctly" $
      (gln "0614141" "18133" "9" :: EitherLE) `shouldBe` Right (GLN "0614141" "18133" "9")

    it "GLN is verified correctly" $
      (gln "0532132" "14112" "7" :: EitherLE) `shouldBe` Right (GLN "0532132" "14112" "7")

    it "IllegalFormat: Invalid length" $
      (gln "0614141" "1813392322222222222" "2" :: EitherLE) `shouldBe` Left IllegalFormat

    it "IllegalFormat: Invalid length" $
      (gln "" "" "" :: EitherLE) `shouldBe` Left IllegalFormat

    it "IllegalFormat: Invalid character" $
      (gln "0614141" "181ab" "9" :: EitherLE) `shouldBe` Left IllegalFormat

    it "InvalidChecksum"  $
      (gln "0614141" "18133" "5" :: EitherLE) `shouldBe` Left InvalidChecksum

    it "PrettyPrint Location as URI" $
      ppURI (Location (GLN "0614141" "18133" "9")) `shouldBe` "urn:epc:id:sgln:0614141.18133.9"

    it "Returns the proper URI prefix)" $
      uriPrefix (Location (GLN "0614141" "18133" "9")) `shouldBe` "urn:epc:id"

    it "Returns the proper URI quantifier)" $
      uriQuantifier (Location (GLN "0614141" "18133" "9")) `shouldBe` "sgln"

    it "Returns the proper URI quantifier)" $
      uriPayload (Location (GLN "0614141" "18133" "9")) `shouldBe` "0614141.18133.9"
