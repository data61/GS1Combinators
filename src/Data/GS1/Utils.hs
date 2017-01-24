module Data.GS1.Utils (
  revertCamelCase
, mkCamelCase
, mkByName
, parseURI
, parseStr2Time
, parseStr2TimeZone
) where

import           Control.Monad.Error.Lens
import           Control.Monad.Except     (MonadError)
import           Data.Char
import           Data.GS1.EPCISTime
import           Data.List.Split
import qualified Data.Text                as T
import           Data.Time
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

camelCase' :: String -> String
camelCase' []     = []
camelCase' (x:xs) = toUpper x : xs

mkCamelCaseWord :: [String] -> [String]
mkCamelCaseWord sl = camelCase' <$> sl

mkCamelCase :: String -> String
mkCamelCase =  filter (/=' ') . unwords . mkCamelCaseWord . splitOn "_"

-- example format: 2005-04-03T20:33:31.116-06:00
-- |parse the string to UTC time, the time zone information will be merged into the time
parseStr2Time :: (AsEPCISTimeError e, MonadError e m) => String -> m EPCISTime
parseStr2Time s = let parsed = parseTimeM True defaultTimeLocale "%FT%X%Q%z" s :: Maybe EPCISTime in
                      case parsed of
                        Just et -> pure et
                        _       -> throwing _IllegalTimeFormat ()

-- |parse the string and obtain TimeZone,
parseStr2TimeZone :: (AsEPCISTimeError e, MonadError e m) => String -> m TimeZone
parseStr2TimeZone s = let parsed = parseTimeM True defaultTimeLocale "%FT%X%Q%z" s :: Maybe ZonedTime in
                      case parsed of
                        Just t -> let tz = zonedTimeZone t :: TimeZone in
                                      pure tz
                        _      -> throwing _IllegalTimeFormat ()

mkByName :: Read a => String -> Maybe a
mkByName s = readMaybe (mkCamelCase s)

parseURI :: Read a => String -> String -> Maybe a
parseURI s uri = let puri = T.pack uri
                     ps = T.pack s
                     (_, s') = T.breakOn puri ps in
                     if T.unpack s' == s then mkByName . last $ splitOn ":" s
                                                      else Nothing


