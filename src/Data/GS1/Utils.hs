module Data.GS1.Utils (
  camelCase
, revertCamelCase
, mkCamelCase
, mkByName
, parseURI
, either2Maybe
) where

import           Data.Char
import           Data.List.Split
import qualified Data.Text       as T
import           Text.Read

-- |insert underscore for each uppercase letter it encounters
-- and make each uppercase letter to lowercase
insertUs' :: T.Text -> T.Text
insertUs' s = s >>= f
  where f c = if isUpper c then '_' : [toLower c] else [toLower c]


revertCamelCase :: T.Text -> T.Text
revertCamelCase "" = ""
revertCamelCase str = let r = insertUs' str in
                          case r of
                            '_':t -> t
                            _     -> r

{-
XXX - this is a good exercise, the camelCase function can be rewritten using the _head prism and the modify function (called (%~)) https://github.csiro.au/Blockchain/GS1Combinators/blob/master/src/Data/GS1/Utils.hs#L29

another question, do revertCamelCase and camelCase functions form an Iso?
-}

camelCase :: T.Text -> T.Text
camelCase []     = []
camelCase (x:xs) = toUpper x : xs

mkCamelCaseWord' :: [T.Text] -> [T.Text]
mkCamelCaseWord' sl = camelCase <$> sl

mkCamelCase :: T.Text -> T.Text
mkCamelCase =  filter (/=' ') . unwords . mkCamelCaseWord' . splitOn "_"

mkByName :: Read a => T.Text -> Maybe a
mkByName s = readMaybe (mkCamelCase s)

parseURI :: Read a => T.Text -> T.Text -> Maybe a
parseURI s uri = let puri = T.pack uri
                     ps = T.pack s
                     (_, s') = T.breakOn puri ps in
                     if T.unpack s' == s
                        then mkByName . last $ splitOn ":" s
                        else Nothing

-- returns (Just Right) or Nothing
either2Maybe :: Either a b -> Maybe b
either2Maybe (Right x) = Just x
either2Maybe (Left _) = Nothing
