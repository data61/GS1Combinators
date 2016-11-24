module Tests.Location where

import Control.Exception (evaluate)
import Test.Hspec
import Data.GS1.Location

testPassGLN :: Spec
testPassGLN = do
  describe "Location" $ do
    it "GLN is verified correctly" $
      show (gln "0614141" "18133" "9") `shouldBe` "0614141.18133.9"

    it "GLN is verified correctly" $
      show (gln "0532132" "14112" "7") `shouldBe` "0532132.14112.7"

    it "InvalidGLNLengthException is caught" $
      evaluate (gln "0614141" "1813392322222222222" "2") `shouldThrow` anyException

    it "InvalidGLNLengthException is caught" $
      evaluate (gln "" "" "") `shouldThrow` anyException

    it "Exception caused by invalid character is caught" $
      evaluate (gln "0614141" "181ab" "9") `shouldThrow` anyException
