-- |EPCIS 7.4.1
module Data.GS1.EPCISTime where

import Control.Exception (throw)
import Data.List
import Data.GS1.GS1Exception
import Data.Time

-- |join a list of string by delimiter
join :: String -> [String] -> String
join p a = concat (intersperse p a)

-- |converts single digit int into double digit format string
formatSingleDigit :: Int -> String
formatSingleDigit n
  | n < 10    = "0" ++ (show n)
  | otherwise = show n

-- |Consists of Date Time and TimeZone (from Data.Time.LocalTime)
data EPCISTime = EPCISTime Date Time TimeZone
  deriving (Eq)

instance Show EPCISTime where
  show (EPCISTime d t tz) = (show d) ++ " " ++ (show t) ++ " " ++ timeZoneOffsetString tz

-- |Date Year Month Day, represented by Int
data Date = Date Int Int Int
  deriving (Eq)

instance Show Date where
  show (Date y m d) = join "-" ((show y) : (map formatSingleDigit [m, d]))

-- |Time Hour Minutes Seconds, represented by Int
data Time = Time Int Int Int
  deriving (Eq)

instance Show Time where
  show (Time h m s) = join ":" (map formatSingleDigit [h, m, s])

-- |use fromGregorianValid for validating date
date :: Int -> Int -> Int -> Date
date y m d
  | fromGregorianValid (toInteger y) m d == Nothing = throw (InvalidDateException "invalid date")
  | otherwise                                       = Date y m d

-- |Time should not exceed the maximum value (otherwise it's the next day)
time :: Int -> Int -> Int -> Time
time h m s
  | h `elem` [0..23] && m `elem` [0..59] && s `elem` [0..59] = Time h m s
  | otherwise                                                = throw (InvalidTimeException "invalid time")
