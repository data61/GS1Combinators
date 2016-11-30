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

    it "IllegalFormat: Invalid length" $
      ((gln "0614141" "1813392322222222222" "2") :: EitherLG) `shouldBe` Left IllegalFormat

    it "IllegalFormat: Invalid length" $
      ((gln "" "" "") :: EitherLG) `shouldBe` Left IllegalFormat

    it "IllegalFormat: Invalid character" $
      ((gln "0614141" "181ab" "9") :: EitherLG) `shouldBe` Left IllegalFormat

    it "InvalidChecksum"  $
      ((gln "0614141" "18133" "5") :: EitherLG) `shouldBe` Left InvalidChecksum
