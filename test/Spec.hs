module Main where

import           Tests.DWhat
import           Tests.DWhen
import           Tests.DWhere
import           Tests.DWhy
import           Tests.Parser
import           Tests.Utils

import           Test.Hspec   (hspec)

main :: IO ()
main = do
  -- Utils
  hspec testRevertCamelCase
  hspec testMkCamelCase
  hspec testParseURI

  -- DWhere
  hspec testReadSGLN
  hspec testPrintSGLN

  -- DWhat
  hspec testBizStep
  hspec testBizTransaction

  -- -- DWhy
  hspec testDisposition

  -- DWhen
  hspec testParseTime

  -- Parser
  hspec testParser
