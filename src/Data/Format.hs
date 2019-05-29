-- | Copied and reduced from https://raw.githubusercontent.com/haskell/time/master/lib/Data/Format.hs
-- Commit: 50fb131

module Data.Format
    ( Productish(..)
    , Format(..)
    , formatShow
    , isoMap
    , literalFormat
    , mandatorySignFormat
    , SignOption(..)
    , integerFormat
    ) where

import           Data.Char
import           Text.ParserCombinators.ReadP

class IsoVariant f where
    isoMap :: (a -> b) -> (b -> a) -> f a -> f b

infixr 3 <**>, **>, <**

class IsoVariant f => Productish f where
    pUnit :: f ()
    (<**>) :: f a -> f b -> f (a, b)
    (**>) :: f () -> f a -> f a
    fu **> fa = isoMap (\((), a) -> a) (\a -> ((), a)) $ fu <**> fa
    (<**) :: f a -> f () -> f a
    fa <** fu = isoMap (\(a, ()) -> a) (\a -> (a, ())) $ fa <**> fu

-- | A text format for a type
data Format t = MkFormat
    { formatShowM :: t -> Maybe String
        -- ^ Show a value in the format, if representable
    , formatReadP :: ReadP t
        -- ^ Read a value in the format
    }

-- | Show a value in the format, or error if unrepresentable
formatShow :: Format t -> t -> String
formatShow fmt t =
    case formatShowM fmt t of
        Just str -> str
        Nothing  -> error "formatShow: bad value"

instance IsoVariant Format where
    isoMap ab ba (MkFormat sa ra) = MkFormat (\b -> sa $ ba b) (fmap ab ra)

instance Productish Format where
    pUnit = MkFormat {formatShowM = \_ -> Just "", formatReadP = return ()}
    (<**>) (MkFormat sa ra) (MkFormat sb rb) = let
        sab (a, b) = do
            astr <- sa a
            bstr <- sb b
            return $ astr ++ bstr
        rab = do
            a <- ra
            b <- rb
            return (a, b)
        in MkFormat sab rab
    (MkFormat sa ra) **> (MkFormat sb rb) = let
        s b = do
            astr <- sa ()
            bstr <- sb b
            return $ astr ++ bstr
        r = do
            ra
            rb
        in MkFormat s r
    (MkFormat sa ra) <** (MkFormat sb rb) = let
        s a = do
            astr <- sa a
            bstr <- sb ()
            return $ astr ++ bstr
        r = do
            a <- ra
            rb
            return a
        in MkFormat s r

literalFormat :: String -> Format ()
literalFormat s = MkFormat {formatShowM = \_ -> Just s, formatReadP = string s >> return ()}

casesFormat :: Eq a => [(a, String)] -> Format a
casesFormat pairs = let
    s t = lookup t pairs
    r []            = pfail
    r ((v, str):pp) = (string str >> return v) <++ r pp
    in MkFormat s $ r pairs

mandatorySignFormat :: (Eq t, Num t) => Format t
mandatorySignFormat = casesFormat [(1, "+"), (0, "+"), (-1, "-")]

data SignOption
    = NoSign
    | NegSign
    | PosNegSign

readSign :: Num t => SignOption -> ReadP (t -> t)
readSign NoSign     = return id
readSign NegSign    = option id $ char '-' >> return negate
readSign PosNegSign = (char '+' >> return id) +++ (char '-' >> return negate)

readNumber :: (Num t, Read t) => SignOption -> Maybe Int -> Bool -> ReadP t
readNumber signOpt mdigitcount allowDecimal = do
    sign <- readSign signOpt
    digits <-
        case mdigitcount of
            Just digitcount -> count digitcount $ satisfy isDigit
            Nothing         -> many1 $ satisfy isDigit
    moredigits <-
        case allowDecimal of
            False -> return ""
            True ->
                option "" $ do
                    _ <- char '.' +++ char ','
                    dd <- many1 (satisfy isDigit)
                    return $ '.' : dd
    return $ sign $ read $ digits ++ moredigits

zeroPad :: Maybe Int -> String -> String
zeroPad Nothing s  = s
zeroPad (Just i) s = replicate (i - length s) '0' ++ s

trimTrailing :: String -> String
trimTrailing "" = ""
trimTrailing "." = ""
trimTrailing s
    | last s == '0' = trimTrailing $ init s
trimTrailing s = s

showNumber :: Show t => SignOption -> Maybe Int -> t -> Maybe String
showNumber signOpt mdigitcount t = let
    showIt str = let
        (intPart, decPart) = break ((==) '.') str
        in (zeroPad mdigitcount intPart) ++ trimTrailing decPart
    in case show t of
           ('-':str) ->
               case signOpt of
                   NoSign -> Nothing
                   _      -> Just $ '-' : showIt str
           str ->
               Just $
               case signOpt of
                   PosNegSign -> '+' : showIt str
                   _          -> showIt str

integerFormat :: (Show t, Read t, Num t) => SignOption -> Maybe Int -> Format t
integerFormat signOpt mdigitcount = MkFormat (showNumber signOpt mdigitcount) (readNumber signOpt mdigitcount False)
