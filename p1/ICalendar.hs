{-# LANGUAGE NamedFieldPuns #-}

module ICalendar where

import Prelude hiding ((<*), (*>), (<$))

import Control.Monad (replicateM)
import Data.Char (isUpper)
import Data.Maybe (listToMaybe)

import ParseLib.Abstract

data DateTime = DateTime
    { date :: Date
    , time :: Time
    , utc :: Bool
    } deriving (Eq, Ord)

printDigits :: Int -> Int -> String
printDigits n = printf $ "%0" ++ show n ++ "d"

printDate :: Date -> String
printDate (Date y m d) =
    concat
        [ printDigits 4 $ unYear y
        , printDigits 2 $ unMonth m
        , printDigits 2 $ unDay d
        ]

printTime :: Time -> String
printTime (Time h m s) =
    concat
        [ printDigits 2 $ unHour h
        , printDigits 2 $ unMinute m
        , printDigits 2 $ unSecond s
        ]

printDateTime :: DateTime -> String
printDateTime (DateTime d t u) =
    concat
        [ printDate d
        , "T"
        , printTime t
        , if u then "Z" else ""
        ]

data Date = Date
    { year  :: Year
    , month :: Month
    , day   :: Day
    } deriving (Eq, Ord)

newtype Year = Year { unYear :: Int }
    deriving (Eq, Ord)

newtype Month = Month { unMonth :: Int }
    deriving (Eq, Ord)

newtype Day = Day { unDay :: Int }
    deriving (Eq, Ord)

data Time = Time
    { hour :: Hour
    , minute :: Minute
    , second :: Second
    } deriving (Eq, Ord)

newtype Hour = Hour { unHour :: Int }
    deriving (Eq, Ord)

newtype Minute = Minute { unMinute :: Int }
    deriving (Eq, Ord)

newtype Second = Second { unSecond :: Int }
    deriving (Eq, Ord)

data Calendar = Calendar
    { prodId :: String
    , events :: [VEvent]
    } deriving Eq

data VEvent = VEvent
    { dtStamp     :: DateTime
    , uid         :: String
    , dtStart     :: DateTime
    , dtEnd       :: DateTime
    , description :: Maybe String
    , summary     :: Maybe String
    , location    :: Maybe String
    } deriving Eq


run :: Parser a b -> [a] -> Maybe b
run p s = listToMaybe [p | (p, []) <- parse p s]

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar


-- "Main" block, DO NOT EDIT.
-- If you want to run the parser + pretty-printing, rename this module (first line) to "Main".
-- DO NOT forget to rename the module back to "ICalendar" before submitting to DomJudge.
main = do
    res <- readCalendar "examples/rooster_infotc.ics"
    putStrLn . render $ maybe (text "Calendar parsing error") (ppMonth (Year 2012) (Month 11)) res
    where
        -- Are these functions supposed to be in the prelude?
        render = undefined
        text = undefined


-- Exercise 1

fromDigits :: [Int] -> Int
fromDigits = foldl (\r d -> r * 10 + d) 0

parseDigits :: Int -> Parser Char Int
parseDigits n = fromDigits <$> n `replicateM` newdigit

parseDate :: Parser Char Date
parseDate = Date
    <$> (Year <$> parseDigits 4)
    <*> (Month <$> parseDigits 2)
    <*> (Day <$> parseDigits 2)

parseTime :: Parser Char Time
parseTime = Time
    <$> (Hour <$> parseDigits 2)
    <*> (Minute <$> parseDigits 2)
    <*> (Second <$> parseDigits 2)

parseDateTime :: Parser Char DateTime
parseDateTime = DateTime
    <$> parseDate
    <*  symbol 'T'
    <*> parseTime
    <*> (isJust <$> optional (symbol 'Z'))

data Token
    = Begin String
    | End String
    | Property String String
    deriving (Eq, Ord, Show)

crlf :: Parser Char String
crlf = token "\r\n"

scanProperty :: Parser Char Token
scanProperty = Property
    <$> some (satisfy isUpper) <* symbol ':'
    <*> some (satisfy (`notElem` ['\r', '\n']))
    <* crlf

scanBegin :: Parser Char Token
scanBegin = Begin <$> (token "BEGIN:" *> some (satisfy isUpper)) <* crlf

scanEnd :: Parser Char Token
scanEnd = End <$> (token "END:" *> some (satisfy isUpper)) <* crlf

scanCalendar :: Parser Char [Token]
scanCalendar = many $ choice
    [ scanBegin
    , scanEnd
    , scanProperty
    ]

mapMaybe :: (a -> Maybe b) -> Parser s a -> Parser s b
mapMaybe f p = p >>= \x ->
    case f x of
        Just y -> succeed y
        Nothing -> empty

satisfyMap :: (s -> Maybe a) -> Parser s a
satisfyMap f = mapMaybe f anySymbol

property :: String -> Parser Token String
property p = satisfyMap $ \t ->
    case t of
        Property p' s
            | p' == p -> Just s
            | otherwise -> Nothing
        _ -> Nothing

begin :: String -> Parser Token ()
begin p = satisfy (== Begin p)

end :: String -> Parser Token ()
end p = satisfy (== End p)

parseCalprops :: Parser Token String
parseCalprops =
    (prodId <* version) <|> (version *> prodId)
    where
        prodId = property "PRODID"
        version = do
            version <- property "VERSION"
            if version == "2.0" then succeed () else empty

parseEvent :: Parser Token VEvent
parseEvent = undefined

parseCalendar :: Parser Token Calendar
parseCalendar = do
    begin "VCALENDAR"
    prodId <- parseCalprops
    events <- many parseEvent
    end "VCALENDAR"
    return Calendar { prodId, events }

-- Exercise 2
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar = undefined


-- Exercise 3
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar = undefined


-- Exercise 4
countEvents :: Calendar -> Int
countEvents = undefined

findEvents :: DateTime -> Calendar -> [VEvent]
findEvents = undefined

checkOverlapping :: Calendar -> Bool
checkOverlapping = undefined

timeSpent :: String -> Calendar -> Int
timeSpent = undefined



-- Exercise 5
ppMonth :: Year -> Month -> Calendar -> String
ppMonth = undefined
