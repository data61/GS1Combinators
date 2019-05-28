module Data.Time.Format.ISO8601
    (
        -- * Format
      Format
    , formatShowM
    , formatShow
    , formatReadP
    , formatParseM
        -- * Common formats
    , ISO8601(..)
    , iso8601Show
    , iso8601ParseM
        -- * All formats
    , FormatExtension(..)
    , formatReadPExtension
    , parseFormatExtension
    , calendarFormat
    , yearMonthFormat
    , yearFormat
    , centuryFormat
    , expandedCalendarFormat
    , expandedYearMonthFormat
    , expandedYearFormat
    , expandedCenturyFormat
    , ordinalDateFormat
    , expandedOrdinalDateFormat
    , weekDateFormat
    , yearWeekFormat
    , expandedWeekDateFormat
    , expandedYearWeekFormat
    , timeOfDayFormat
    , withTimeDesignator
    , withUTCDesignator
    , timeOffsetFormat
    , timeOfDayAndOffsetFormat
    , localTimeFormat
    , zonedTimeFormat
    , utcTimeFormat
    , dayAndTimeFormat
    , timeAndOffsetFormat
    , intervalFormat
    , recurringIntervalFormat
    ) where

import           Control.Monad.Fail
import           Data.Fixed
import           Data.Format
import           Data.Time
import           Data.Time.Calendar.OrdinalDate
-- import           Data.Time.Calendar.Private
import           Data.Time.Calendar.WeekDate
import           Prelude
import           Text.ParserCombinators.ReadP

data FormatExtension
    =
    -- | ISO 8601:2004(E) sec. 2.3.4. Use hyphens and colons.
      ExtendedFormat
    -- | ISO 8601:2004(E) sec. 2.3.3. Omit hyphens and colons. "The basic format should be avoided in plain text."
    | BasicFormat

-- | Read a value in either extended or basic format
formatReadPExtension :: (FormatExtension -> Format t) -> ReadP t
formatReadPExtension ff = formatReadP (ff ExtendedFormat) +++ formatReadP (ff BasicFormat)

-- | Parse a value in either extended or basic format
parseFormatExtension :: (MonadFail m) => (FormatExtension -> Format t) -> String -> m t
parseFormatExtension ff = parseReader $ formatReadPExtension ff

sepFormat :: String -> Format a -> Format b -> Format (a, b)
sepFormat sep fa fb = (fa <** literalFormat sep) <**> fb

dashFormat :: Format a -> Format b -> Format (a, b)
dashFormat = sepFormat "-"

colnFormat :: Format a -> Format b -> Format (a, b)
colnFormat = sepFormat ":"

extDashFormat :: FormatExtension -> Format a -> Format b -> Format (a, b)
extDashFormat ExtendedFormat = dashFormat
extDashFormat BasicFormat    = (<**>)

extColonFormat :: FormatExtension -> Format a -> Format b -> Format (a, b)
extColonFormat ExtendedFormat = colnFormat
extColonFormat BasicFormat    = (<**>)

expandedYearFormat' :: Int -> Format Integer
expandedYearFormat' n = integerFormat PosNegSign (Just n)

yearFormat' :: Format Integer
yearFormat' = integerFormat NegSign (Just 4)

monthFormat :: Format Int
monthFormat = integerFormat NoSign (Just 2)

dayOfMonthFormat :: Format Int
dayOfMonthFormat = integerFormat NoSign (Just 2)

dayOfYearFormat :: Format Int
dayOfYearFormat = integerFormat NoSign (Just 3)

weekOfYearFormat :: Format Int
weekOfYearFormat = literalFormat "W" **> integerFormat NoSign (Just 2)

dayOfWeekFormat :: Format Int
dayOfWeekFormat = integerFormat NoSign (Just 1)

hourFormat' :: Format Int
hourFormat' = integerFormat NoSign (Just 2)

data E14

instance HasResolution E14 where
    resolution _ = 100000000000000

data E16

instance HasResolution E16 where
    resolution _ = 10000000000000000

minuteFormat :: Format Int
minuteFormat = integerFormat NoSign (Just 2)

secondFormat :: Format Pico
secondFormat = decimalFormat NoSign (Just 2)

mapGregorian :: Format (Integer, (Int, Int)) -> Format Day
mapGregorian =
    mapMFormat (\(y, (m, d)) -> fromGregorianValid y m d) (\day -> (\(y, m, d) -> Just (y, (m, d))) $ toGregorian day)

mapOrdinalDate :: Format (Integer, Int) -> Format Day
mapOrdinalDate = mapMFormat (\(y, d) -> fromOrdinalDateValid y d) (Just . toOrdinalDate)

mapWeekDate :: Format (Integer, (Int, Int)) -> Format Day
mapWeekDate =
    mapMFormat (\(y, (w, d)) -> fromWeekDateValid y w d) (\day -> (\(y, w, d) -> Just (y, (w, d))) $ toWeekDate day)

mapTimeOfDay :: Format (Int, (Int, Pico)) -> Format TimeOfDay
mapTimeOfDay = mapMFormat (\(h, (m, s)) -> makeTimeOfDayValid h m s) (\(TimeOfDay h m s) -> Just (h, (m, s)))

-- | ISO 8601:2004(E) sec. 4.1.2.2
calendarFormat :: FormatExtension -> Format Day
calendarFormat fe = mapGregorian $ extDashFormat fe yearFormat $ extDashFormat fe monthFormat dayOfMonthFormat

-- | ISO 8601:2004(E) sec. 4.1.2.3(a)
yearMonthFormat :: Format (Integer, Int)
yearMonthFormat = yearFormat <**> literalFormat "-" **> monthFormat

-- | ISO 8601:2004(E) sec. 4.1.2.3(b)
yearFormat :: Format Integer
yearFormat = yearFormat'

-- | ISO 8601:2004(E) sec. 4.1.2.3(c)
centuryFormat :: Format Integer
centuryFormat = integerFormat NegSign (Just 2)

-- | ISO 8601:2004(E) sec. 4.1.2.4(a)
expandedCalendarFormat :: Int -> FormatExtension -> Format Day
expandedCalendarFormat n fe =
    mapGregorian $ extDashFormat fe (expandedYearFormat n) $ extDashFormat fe monthFormat dayOfMonthFormat

-- | ISO 8601:2004(E) sec. 4.1.2.4(b)
expandedYearMonthFormat :: Int -> Format (Integer, Int)
expandedYearMonthFormat n = dashFormat (expandedYearFormat n) monthFormat

-- | ISO 8601:2004(E) sec. 4.1.2.4(c)
expandedYearFormat :: Int -> Format Integer
expandedYearFormat = expandedYearFormat'

-- | ISO 8601:2004(E) sec. 4.1.2.4(d)
expandedCenturyFormat :: Int -> Format Integer
expandedCenturyFormat n = integerFormat PosNegSign (Just n)

-- | ISO 8601:2004(E) sec. 4.1.3.2
ordinalDateFormat :: FormatExtension -> Format Day
ordinalDateFormat fe = mapOrdinalDate $ extDashFormat fe yearFormat dayOfYearFormat

-- | ISO 8601:2004(E) sec. 4.1.3.3
expandedOrdinalDateFormat :: Int -> FormatExtension -> Format Day
expandedOrdinalDateFormat n fe = mapOrdinalDate $ extDashFormat fe (expandedYearFormat n) dayOfYearFormat

-- | ISO 8601:2004(E) sec. 4.1.4.2
weekDateFormat :: FormatExtension -> Format Day
weekDateFormat fe = mapWeekDate $ extDashFormat fe yearFormat $ extDashFormat fe weekOfYearFormat dayOfWeekFormat

-- | ISO 8601:2004(E) sec. 4.1.4.3
yearWeekFormat :: FormatExtension -> Format (Integer, Int)
yearWeekFormat fe = extDashFormat fe yearFormat weekOfYearFormat

-- | ISO 8601:2004(E) sec. 4.1.4.2
expandedWeekDateFormat :: Int -> FormatExtension -> Format Day
expandedWeekDateFormat n fe =
    mapWeekDate $ extDashFormat fe (expandedYearFormat n) $ extDashFormat fe weekOfYearFormat dayOfWeekFormat

-- | ISO 8601:2004(E) sec. 4.1.4.3
expandedYearWeekFormat :: Int -> FormatExtension -> Format (Integer, Int)
expandedYearWeekFormat n fe = extDashFormat fe (expandedYearFormat n) weekOfYearFormat

-- | ISO 8601:2004(E) sec. 4.2.2.2, 4.2.2.4(a)
timeOfDayFormat :: FormatExtension -> Format TimeOfDay
timeOfDayFormat fe = mapTimeOfDay $ extColonFormat fe hourFormat' $ extColonFormat fe minuteFormat secondFormat

-- | ISO 8601:2004(E) sec. 4.2.2.5
withTimeDesignator :: Format t -> Format t
withTimeDesignator f = literalFormat "T" **> f

-- | ISO 8601:2004(E) sec. 4.2.4
withUTCDesignator :: Format t -> Format t
withUTCDesignator f = f <** literalFormat "Z"

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

-- | ISO 8601:2004(E) sec. 4.2.5.2
timeOfDayAndOffsetFormat :: FormatExtension -> Format (TimeOfDay, TimeZone)
timeOfDayAndOffsetFormat fe = timeOfDayFormat fe <**> timeOffsetFormat fe

-- | ISO 8601:2004(E) sec. 4.3.2
localTimeFormat :: Format Day -> Format TimeOfDay -> Format LocalTime
localTimeFormat fday ftod =
    isoMap (\(day, tod) -> LocalTime day tod) (\(LocalTime day tod) -> (day, tod)) $ fday <**> withTimeDesignator ftod

-- | ISO 8601:2004(E) sec. 4.3.2
zonedTimeFormat :: Format Day -> Format TimeOfDay -> FormatExtension -> Format ZonedTime
zonedTimeFormat fday ftod fe =
    isoMap (\(lt, tz) -> ZonedTime lt tz) (\(ZonedTime lt tz) -> (lt, tz)) $
    timeAndOffsetFormat (localTimeFormat fday ftod) fe

-- | ISO 8601:2004(E) sec. 4.3.2
utcTimeFormat :: Format Day -> Format TimeOfDay -> Format UTCTime
utcTimeFormat fday ftod =
    isoMap (localTimeToUTC utc) (utcToLocalTime utc) $ withUTCDesignator $ localTimeFormat fday ftod

-- | ISO 8601:2004(E) sec. 4.3.3
dayAndTimeFormat :: Format Day -> Format time -> Format (Day, time)
dayAndTimeFormat fday ft = fday <**> withTimeDesignator ft

-- | ISO 8601:2004(E) sec. 4.3.3
timeAndOffsetFormat :: Format t -> FormatExtension -> Format (t, TimeZone)
timeAndOffsetFormat ft fe = ft <**> timeOffsetFormat fe

-- | ISO 8601:2004(E) sec. 4.4.4.1
intervalFormat :: Format a -> Format b -> Format (a, b)
intervalFormat = sepFormat "/"

-- | ISO 8601:2004(E) sec. 4.5
recurringIntervalFormat :: Format a -> Format b -> Format (Int, a, b)
recurringIntervalFormat fa fb =
    isoMap (\(r, (a, b)) -> (r, a, b)) (\(r, a, b) -> (r, (a, b))) $
    sepFormat "/" (literalFormat "R" **> integerFormat NoSign Nothing) $ intervalFormat fa fb

class ISO8601 t where
    -- | The most commonly used ISO 8601 format for this type.
    iso8601Format :: Format t

-- | Show in the most commonly used ISO 8601 format.
iso8601Show :: ISO8601 t => t -> String
iso8601Show = formatShow iso8601Format

-- | Parse the most commonly used ISO 8601 format.
iso8601ParseM :: (MonadFail m, ISO8601 t) => String -> m t
iso8601ParseM = formatParseM iso8601Format

-- | @±hh:mm@ (ISO 8601:2004(E) sec. 4.2.5.1 extended format)
instance ISO8601 TimeZone where
    iso8601Format = timeOffsetFormat ExtendedFormat
