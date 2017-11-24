module Tests.DWhere where

import           Data.GS1.EPC
import           Data.GS1.DWhere
import           Data.GS1.URI
import           Test.Hspec

type EitherLE = Either LocationError LocationEPC
--                                  Experimental
testSGLN :: Spec
testSGLN =
  describe "Location" $ do
    describe "SGLN with location reference" $ do
      it "LocationReferenceNum with extension" $
        readURI "urn:epc:id:sgln:0614141.12345.400" `shouldBe` 
          Just (SGLN "0614141" (LocationReferenceNum "12345") (Just "400"))
      it "LocationReferenceNum without extension" $
        readURI "urn:epc:id:sgln:0614141.12345" `shouldBe`
          Just (SGLN "0614141" (LocationReferenceNum "12345") Nothing)
      it "Invalid URI" $
        (readURI :: String -> Maybe LocationEPC) "this:is:invalid" `shouldBe` Nothing
      -- it "Some other valid uri" $
      --   readURI "urn:epc:class:lgtin:4012345.012345.998877" `shouldBe` Nothing
      -- it "Empty string" $
      --   readURI "" `shouldBe` Nothing
      -- it "Only one component missing" $
      --   readURI "urn:epc:sgln:0614141.12345.400" `shouldBe` Nothing
      -- it "Invalid length - failure expected" $
      --   readURI "urn:epc:id:sgln:06.12.4" `shouldBe` Nothing -- failure expected
-- TODO
    -- describe "SGLN with lat and long" $ do
    --   it "LatLong" $
    --     readURI "urn:epc:id:sgln:0614141.12345.400" `shouldBe` 
    --       Just (SGLN "0614141" (LocationReferenceNum "12345") (Just "400"))
      
    --   it "LatLong" $
    --     readURI "urn:epc:id:sgln:0614141.12345.400" `shouldBe` 
    --       Just (SGLN "0614141" (LocationReferenceNum "12345") (Just "400"))
    


    -- it "GLN is verified correctly from 20141216 onesteel" $
    --   (gln "9348585" "01002" "3" :: EitherLE) `shouldBe` Right (GLN "9348585" "01002" "3")

    -- -- the GLN is incorrect on the spreadsheet as it contains 12 digits only
    -- it "GLN is verified correctly from 20141216 onesteel" $
    --   (gln "9348585" "08001" "9" :: EitherLE) `shouldBe` Right (GLN "9348585" "08001" "9")

    -- it "IllegalFormat: Invalid length" $
    --   (gln "0614141" "1813392322222222222" "2" :: EitherLE) `shouldBe` Left IllegalGLNFormat

    -- it "IllegalFormat: Invalid length" $
    --   (gln "" "" "" :: EitherLE) `shouldBe` Left IllegalGLNFormat

    -- it "IllegalFormat: Invalid character" $
    --   (gln "0614141" "181ab" "9" :: EitherLE) `shouldBe` Left IllegalGLNFormat

    -- it "InvalidChecksum"  $
    --   (gln "0614141" "18133" "5" :: EitherLE) `shouldBe` Left InvalidChecksum

    -- it "PrettyPrint Location as URI" $
    --   ppURI (Location (GLN "0614141" "18133" "9")) `shouldBe` "urn:epc:id:sgln:0614141.18133.9"

    -- it "Returns the proper URI prefix" $
    --   uriPrefix (Location (GLN "0614141" "18133" "9")) `shouldBe` "urn:epc:id"

    -- it "Returns the proper URI quantifier" $
    --   uriQuantifier (Location (GLN "0614141" "18133" "9")) `shouldBe` "sgln"

    -- it "Returns the proper URI quantifier" $
    --   uriPayload (Location (GLN "0614141" "18133" "9")) `shouldBe` "0614141.18133.9"

-- testMkEPC :: Spec
-- testMkEPC =
--   describe "EPC" $ do
--     it "make a GLN from valid string" $
--       mkEPC "GLN" "0532132.14112.7" `shouldBe` Just (GLN "0532132" "14112" "7")

--     it "make a GLN from valid string" $
--       mkEPC "GLN" "0614141.18133.9" `shouldBe` Just (GLN "0614141" "18133" "9")

--     it "make Nothing from invalid string" $
--       mkEPC "GLN" "0614141.18133.5" `shouldBe` Nothing

--     it "make Nothing from invalid string" $
--       mkEPC "GLN" "0614141.18133.5121" `shouldBe` Nothing
    
--     it "make Nothing from invalid string" $
--       mkEPC "GLN" "really bad input" `shouldBe` Nothing
