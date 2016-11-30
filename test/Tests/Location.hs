module Tests.Location where

import Test.Hspec
import Data.GS1.Location

type EitherLG = Either LocationError GLN

testPassGLN :: Spec
testPassGLN = do
  describe "Location" $ do
    it "GLN is verified correctly" $
      show ((gln "0614141" "18133" "9") :: EitherLG) `shouldBe` "Right 0614141.18133.9"

    it "GLN is verified correctly" $
      show ((gln "0532132" "14112" "7") :: EitherLG) `shouldBe` "Right 0532132.14112.7"

    it "InvalidGLNLengthException is caught" $
      show ((gln "0614141" "1813392322222222222" "2") :: EitherLG) `shouldBe` "Left GLNInvalid"

    it "InvalidGLNLengthException is caught" $
      show ((gln "" "" "") :: EitherLG) `shouldBe` "Left GLNInvalid"

    it "Exception caused by invalid character is caught" $
      show ((gln "0614141" "181ab" "9") :: EitherLG) `shouldBe` "Left GLNInvalid"
