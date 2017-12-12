module Main where

import           Tests.DWhat
import           Tests.DWhen
import           Tests.DWhere
import           Tests.DWhy
import           Tests.Parser -- it's not being used right now
import           Tests.Utils

import           Test.Hspec   (hspec)

main :: IO ()
main = do
  -- DWhere
  hspec testReadSGLN
  hspec testPrintSGLN

  -- DWhat
  hspec testBizStep
  hspec testBizTransaction
  hspec testPpDWhat

  -- -- DWhy
  hspec testDisposition

  -- DWhen
  hspec testParseTime
  hspec testMkDWhen

  -- Parser
  hspec testParser

  -- Utils
  hspec testRevertCamelCase
  hspec testMkCamelCase
  hspec testParseURI
