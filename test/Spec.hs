module Main where

import Tests.Location
import Tests.EPCISTime

import Test.Hspec (hspec)

main :: IO ()
main = do
  hspec testPassGLN
  hspec testTime
