module Main where

import Tests.Event
import Tests.Location

import Test.Hspec (hspec)

main :: IO ()
main = do
  hspec testPassGLN
  hspec testBizStep
  hspec testDisposition
  hspec testBizTransaction
