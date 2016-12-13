module Data.GS1.Utils (
  revertCamelCase
) where

import Data.Char

-- |insert underscore for each uppercase letter it encounters
-- and make each uppercase letter to lowercase
insertUs' :: String -> String
insertUs' [] = []
insertUs' (x:xs)
  | isUpper x = '_':(toLower x):(insertUs' xs)
  | otherwise = x:(insertUs' xs)
                           
revertCamelCase :: String -> String
revertCamelCase [] = []
revertCamelCase str = let r = insertUs' str
                          h = head r in
                          case h of
                            '_' -> tail r
                            _   -> r
