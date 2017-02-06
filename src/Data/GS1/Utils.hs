module Data.GS1.Utils (
  camelCase
, revertCamelCase
, mkCamelCase
, mkByName
, parseURI
) where

import           Data.Char
import           Data.List.Split
import qualified Data.Text       as T
import           Text.Read

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

camelCase :: String -> String
camelCase []     = []
camelCase (x:xs) = toUpper x : xs

mkCamelCaseWord' :: [String] -> [String]
mkCamelCaseWord' sl = camelCase <$> sl

mkCamelCase :: String -> String
mkCamelCase =  filter (/=' ') . unwords . mkCamelCaseWord' . splitOn "_"

mkByName :: Read a => String -> Maybe a
mkByName s = readMaybe (mkCamelCase s)

parseURI :: Read a => String -> String -> Maybe a
parseURI s uri = let puri = T.pack uri
                     ps = T.pack s
                     (_, s') = T.breakOn puri ps in
                     if T.unpack s' == s then mkByName . last $ splitOn ":" s
                                                      else Nothing
