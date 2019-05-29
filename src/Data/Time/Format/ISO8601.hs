-- | Copied and reduced from https://raw.githubusercontent.com/haskell/time/master/lib/Data/Time/Format/ISO8601.hs
-- Commit: 50fb131

module Data.Time.Format.ISO8601 (iso8601Show) where

import           Data.Fixed
import           Data.Format
import           Data.Time

data FormatExtension
    =
    -- | ISO 8601:2004(E) sec. 2.3.4. Use hyphens and colons.
      ExtendedFormat
    -- | ISO 8601:2004(E) sec. 2.3.3. Omit hyphens and colons. "The basic format should be avoided in plain text."
    | BasicFormat

sepFormat :: String -> Format a -> Format b -> Format (a, b)
sepFormat sep fa fb = (fa <** literalFormat sep) <**> fb

colnFormat :: Format a -> Format b -> Format (a, b)
colnFormat = sepFormat ":"

extColonFormat :: FormatExtension -> Format a -> Format b -> Format (a, b)
extColonFormat ExtendedFormat = colnFormat
extColonFormat BasicFormat    = (<**>)

data E14

instance HasResolution E14 where
    resolution _ = 100000000000000

data E16

instance HasResolution E16 where
    resolution _ = 10000000000000000

-- | ISO 8601:2004(E) sec. 4.2.5.1
timeOffsetFormat :: FormatExtension -> Format TimeZone
timeOffsetFormat fe = let
    toTimeZone (sign, (h, m)) = minutesToTimeZone $ sign * (h * 60 + m)
    fromTimeZone tz = let
        mm = timeZoneMinutes tz
        hm = quotRem (abs mm) 60
        in (signum mm, hm)
    in isoMap toTimeZone fromTimeZone $
       mandatorySignFormat <**> extColonFormat fe (integerFormat NoSign (Just 2)) (integerFormat NoSign (Just 2))

class ISO8601 t where
    -- | The most commonly used ISO 8601 format for this type.
    iso8601Format :: Format t

-- | Show in the most commonly used ISO 8601 format.
iso8601Show :: ISO8601 t => t -> String
iso8601Show = formatShow iso8601Format

-- | @±hh:mm@ (ISO 8601:2004(E) sec. 4.2.5.1 extended format)
instance ISO8601 TimeZone where
    iso8601Format = timeOffsetFormat ExtendedFormat
