module Data.GS1.Utils (
  revertCamelCase
, mkCamelCase
) where

import           Data.Char
import           Data.List.Split
import           Data.String
import qualified Data.Text as T

-- |insert underscore for each uppercase letter it encounters
-- and make each uppercase letter to lowercase
insertUs' :: String -> String
insertUs' [] = []
insertUs' (x:xs)
  | isUpper x = '_' : toLower x : insertUs' xs
  | otherwise = x : insertUs' xs

revertCamelCase :: String -> String
revertCamelCase [] = []
revertCamelCase str = let r = insertUs' str in
                          case r of
                            '_':t -> t
                            _     -> r

mkCamelCaseWord :: [String] -> [String]
mkCamelCaseWord [] = []
mkCamelCaseWord (x:xs) = case x of
                              (y:ys) -> (toUpper y:ys):mkCamelCaseWord xs
                              _      -> mkCamelCaseWord xs

wrapStrip' :: String -> String
wrapStrip' [] = []
wrapStrip' (x:xs) = if x == ' ' then wrapStrip' xs else x:wrapStrip' xs

mkCamelCase :: String -> String
mkCamelCase =  wrapStrip' . unwords . mkCamelCaseWord . splitOn "_"
