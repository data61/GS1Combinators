{-# LANGUAGE OverloadedStrings #-}
module Data.GS1.Utils (
  camelCase
, revertCamelCase
, mkCamelCase
, mkByName
, parseURI
, either2Maybe
, getTotalLength
) where

import           Data.Char
import qualified Data.Text       as T
import           Text.Read

-- |insert underscore for each uppercase letter it encounters
-- and make each uppercase letter to lowercase
-- insertUs' :: T.Text -> T.Text
-- insertUs' s = s >>= f
--   where f c = if isUpper c then '_' : [toLower c] else [toLower c]

func :: Char -> T.Text
func c
  | isUpper c = T.append "_" (T.singleton $ toLower c)
  | otherwise = T.singleton c

insertUs' :: T.Text -> T.Text
insertUs' = T.concatMap func


revertCamelCase :: T.Text -> T.Text
revertCamelCase t
  | T.null t  = T.empty
  | otherwise = let r = insertUs' t in
                    case T.head r of
                      '_' -> T.tail r
                      _     -> r

{-
XXX - this is a good exercise, the camelCase function can be rewritten using
the _head prism and the modify function (called (%~)) 
another question, do revertCamelCase and camelCase functions form an Iso?
-}

camelCase :: T.Text -> T.Text
camelCase = T.toTitle

mkCamelCaseWord' :: [T.Text] -> [T.Text]
mkCamelCaseWord' sl = camelCase <$> sl

mkCamelCase :: T.Text -> T.Text
mkCamelCase =  T.filter (/=' ') . T.unwords . mkCamelCaseWord' . T.splitOn "_"

mkByName :: Read a => T.Text -> Maybe a
mkByName s = readMaybe $ T.unpack $ mkCamelCase s

parseURI :: Read a => T.Text -> T.Text -> Maybe a
parseURI s uri = let(_, s') = T.breakOn uri s in
                    if s' == s
                      then mkByName . last $ T.splitOn ":" s
                      else Nothing

-- returns (Just Right) or Nothing
either2Maybe :: Either a b -> Maybe b
either2Maybe (Right x) = Just x
either2Maybe (Left _) = Nothing

getTotalLength :: [T.Text] -> Int
getTotalLength ts = sum $ T.length <$> ts
