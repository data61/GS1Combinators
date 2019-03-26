{-# LANGUAGE OverloadedLists #-}

-- | module containing various utility functions
-- none of these functions are specific to GS1

module Data.GS1.Utils (
  camelCase
, revertCamelCase
, mkCamelCase
, mkByName
, parseURI
, either2Maybe
, getTotalLength
, optionally
, merge
) where

import           Data.Aeson.Types ( Value(Object), ToJSON(..), Pair )
import           Data.Char
import           Data.Monoid ((<>))
import qualified Data.Text   as T
import           Text.Read

-- TODO: Write some tests for this code and make sure it's safe. A lot could be
-- written more clearly

-- a helper function
-- if it's a capital char, inserts an '_' before
-- else just returns the character
-- sample usage:
-- 'c' returns 'c'
-- 'C' returns '_c'
putUsOrReturnChar :: Char -> T.Text
putUsOrReturnChar c
  | isUpper c = "_" <> [toLower c]
  | otherwise = T.singleton c

insertUs :: T.Text -> T.Text
insertUs = T.concatMap putUsOrReturnChar


revertCamelCase :: T.Text -> T.Text
revertCamelCase t
  | T.null t  = T.empty
  | otherwise = let r = insertUs t in
                    case T.head r of
                      '_' -> T.tail r
                      _   -> r

{-
XXX - this is a good exercise, the camelCase function can be rewritten using
the _head prism and the modify function (called (%~))
another question, do revertCamelCase and camelCase functions form an Iso?

Great question
-}

camelCase :: T.Text -> T.Text
camelCase = T.toTitle

-- Does this need to be a function? it's just fmap camelCase
mkCamelCaseWord :: [T.Text] -> [T.Text]
mkCamelCaseWord sl = camelCase <$> sl

mkCamelCase :: T.Text -> T.Text
mkCamelCase =  T.filter (/=' ') . T.unwords . mkCamelCaseWord . T.splitOn "_"

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
either2Maybe (Left _)  = Nothing

getTotalLength :: [T.Text] -> Int
getTotalLength ts = sum $ T.length <$> ts

-- Useful when writing ToJSON instances and only want to include a field if it's not Nothing
optionally :: ToJSON a => T.Text -> Maybe a -> [Pair]
optionally _ Nothing = []
optionally k (Just v) = [ (k, toJSON v) ]

merge :: Value -> Value -> Value
merge (Object a) (Object b) = Object (a <> b)
merge a _ = a
