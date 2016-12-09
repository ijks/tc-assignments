{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module ICalendar where

import Prelude hiding ((<*), (*>), (<$), sequence)

import Control.Monad (replicateM)
import Data.Char (isUpper)
import Data.List
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import System.IO
import Text.Printf (printf)

import ParseLib.Abstract
import qualified Text.PrettyPrint as PP

data DateTime = DateTime
    { date :: Date
    , time :: Time
    , utc :: Bool
    } deriving (Eq, Ord)

-- instance Show DateTime where
--     show = printDateTime

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

-- instance Show Calendar where
--     show = printCalendar

data VEvent = VEvent
    { dtStamp     :: DateTime
    , uid         :: String
    , dtStart     :: DateTime
    , dtEnd       :: DateTime
    , description :: Maybe String
    , summary     :: Maybe String
    , location    :: Maybe String
    } deriving Eq

-- instance Show VEvent where
--     show = printVEvent

run :: Parser a b -> [a] -> Maybe b
run p s = listToMaybe [res | (res, []) <- parse p s]

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- "Main" block, DO NOT EDIT.
-- If you want to run the parser + pretty-printing, rename this module (first line) to "Main".
-- DO NOT forget to rename the module back to "ICalendar" before submitting to DomJudge.
main :: IO ()
main = do
    res <- readCalendar "examples/bastille.ics"
    putStrLn $ maybe "Calendar parsing error"
        (ppMonth (Year 2012) (Month 11)) res

-- Exercise 1

data Token
    = Begin String
    | End String
    | Property String
    | Text String
    | DT DateTime
    deriving Eq

crlf :: Parser Char String
crlf = token "\r\n"

scanProperty :: Parser Char Token
scanProperty = Property <$> some (satisfy isUpper) <* symbol ':'

scanBegin :: Parser Char Token
scanBegin = Begin <$> (token "BEGIN:" *> some (satisfy isUpper)) <* crlf

scanEnd :: Parser Char Token
scanEnd = End <$> (token "END:" *> some (satisfy isUpper)) <* crlf

scanText :: Parser Char Token
scanText = Text
    <$> some
        ( satisfy (`notElem` ['\r', '\n'])
        <|> token "\r\n" *> (symbol ' ' <|> symbol '\t')
        )
    <* crlf

scanDT :: Parser Char Token
scanDT = DT <$> parseDateTime <* crlf

scanCalendar :: Parser Char [Token]
scanCalendar = many $ choice
    [ scanBegin
    , scanEnd
    , scanProperty
    , scanDT
    , scanText
    ]

property :: String -> Parser Token Token
property p = satisfy (== Property p)

begin :: String -> Parser Token Token
begin p = satisfy (== Begin p)

end :: String -> Parser Token Token
end p = satisfy (== End p)

text :: Parser Token String
text = anySymbol >>= \case
    Text t -> pure t
    _ -> empty

datetime :: Parser Token DateTime
datetime = anySymbol >>= \case
    DT t -> pure t
    _ -> empty

parseCalprops :: Parser Token String
parseCalprops =
    (prodId <* version) <|> (version *> prodId)
    where
        prodId = property "PRODID" *> text
        version = property "VERSION" *> do
            v <- text
            if v == "2.0" then pure () else empty

data Property
    = DTStamp DateTime
    | DTStart DateTime
    | DTEnd DateTime
    | UID String
    | Description String
    | Summary String
    | Location String
    -- deriving (Show)

isDTStamp, isDTStart, isDTEnd, isUID, isDescription, isSummary, isLocation
    :: Property -> Bool
isDTStamp (DTStamp _) = True
isDTStamp _ = False
isDTStart (DTStart _) = True
isDTStart _ = False
isDTEnd (DTEnd _) = True
isDTEnd _ = False
isUID (UID _) = True
isUID _ = False
isDescription (Description _) = True
isDescription _ = False
isSummary (Summary _) = True
isSummary _ = False
isLocation (Location _) = True
isLocation _ = False

parseProperty :: Parser Token Property
parseProperty = choice
    [ DTStamp <$> (property "DTSTAMP" *> datetime)
    , DTStart <$> (property "DTSTART" *> datetime)
    , DTEnd <$> (property "DTEND" *> datetime)
    , UID <$> (property "UID" *> text)
    , Description <$> (property "DESCRIPTION" *> text)
    , Summary <$> (property "SUMMARY" *> text)
    , Location <$> (property "LOCATION" *> text)
    ]

findOne :: (a -> Bool) -> [a] -> Maybe a
findOne p xs = case filter p xs of
    [x] -> Just x
    _ -> Nothing

findOptional :: (a -> Bool) -> [a] -> Maybe (Maybe a)
findOptional p xs = case filter p xs of
    [] -> Just Nothing
    [x] -> Just (Just x)
    _ -> Nothing

makeEvent :: [Property] -> Maybe VEvent
makeEvent ps = do
    let getDescr (Description d) = d
    let getSummary (Summary s) = s
    let getLocation (Location l) = l

    DTStamp dtStamp <- findOne isDTStamp ps
    DTStart dtStart <- findOne isDTStart ps
    DTEnd dtEnd <- findOne isDTEnd ps
    UID uid <- findOne isUID ps
    description <- fmap getDescr <$> findOptional isDescription ps
    summary <-  fmap getSummary <$> findOptional isSummary ps
    location <- fmap getLocation <$> findOptional isLocation ps

    return
        VEvent { dtStamp, dtStart, dtEnd, uid, description, summary, location }

parseEvent :: Parser Token VEvent
parseEvent = begin "VEVENT" *> parseProperties <* end "VEVENT"
    where
        parseProperties = many parseProperty >>= \ps ->
            case makeEvent ps of
                Just e -> pure e
                Nothing -> empty

parseCalendar :: Parser Token Calendar
parseCalendar = do
    begin "VCALENDAR"
    prodId <- parseCalprops
    events <- many parseEvent
    end "VCALENDAR"
    return Calendar { prodId, events }

-- Exercise 2

readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar path = do
    handle <- openFile path ReadMode
    hSetNewlineMode handle noNewlineTranslation
    file <- hGetContents handle
    return $ recognizeCalendar file

-- Exercise 3
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)

printCalendar :: Calendar -> String
printCalendar Calendar { .. } =
    concat $ map (++ "\r\n")
        [ "BEGIN:VCALENDAR"
        , "PRODID:" ++ prodId
        , "VERSION:2.0"
        ]
        ++ map printVEvent events
        ++ ["END:VCALENDAR\r\n"]

printVEvent :: VEvent -> String
printVEvent VEvent { .. } =
    concat $ map (++ "\r\n")
        [ "BEGIN:VEVENT"
        , "UID:" ++ uid
        , "DTSTAMP:" ++ printDateTime dtStamp
        , "DTSTART:" ++ printDateTime dtStart
        , "DTEND:" ++ printDateTime dtEnd
        ] ++
        [ maybeShow "DESCRIPTION:" description
        , maybeShow "SUMMARY:" summary
        , maybeShow "LOCATION:" location
        ]
        ++ ["END:VEVENT\r\n"]
    where
        maybeShow _ Nothing = ""
        maybeShow s (Just a) = s ++ a ++ "\r\n"

-- Exercise 4

countEvents :: Calendar -> Int
countEvents = length . events

findEvents :: DateTime -> Calendar -> [VEvent]
findEvents dt = filter checkDate . events
    where
        checkDate VEvent { dtStart, dtEnd } = dtStart <= dt && dt <= dtEnd

hasOverlapping :: Calendar -> Bool
hasOverlapping = not . null . overlapping . events
    where
        overlapping es =
            [ (a, b) | a <- es , b <- es
            , dtStart a < dtStart b && dtStart b < dtEnd a
            ]

leapYear :: Int -> Bool
leapYear year = (year `mod` 4 == 0)
    && ((year `mod` 400 == 0) || (year `mod` 100 /= 0))

monthLength :: Int -> Int -> Int
monthLength year month =
    [31, if leapYear year then 29 else 28, 31, 30,
     31, 30, 31, 31, 30, 31, 30, 31] !! (month - 1)

days :: Date -> Int
days (Date (Year year) (Month month) (Day day)) =
    day + monthTotal month + yearTotal year
    where
        monthTotal m = sum . fmap (monthLength year) $ [0 .. m]
        yearTotal y = sum . fmap yearLength $ [0 .. y]
        yearLength y = if leapYear year then 366 else 365

seconds :: Time -> Int
seconds (Time (Hour hour) (Minute minute) (Second second)) =
    hour * 60 * 60 + minute * 60 + second

totalSeconds :: DateTime -> Int
totalSeconds (DateTime date time _) =
    days date * 24 * 60 * 60 + seconds time

timeSpent :: String -> Calendar -> Int
timeSpent s = sum . fmap duration . withSummary s . events
    where
        duration VEvent { dtStart, dtEnd } =
            (totalSeconds dtEnd - totalSeconds dtStart) `div` 60
        withSummary s = mapMaybe $ \e -> do
            s' <- summary e
            if s' == s then Just e else Nothing

-- Exercise 5

ppMonth :: Year -> Month -> Calendar -> String
ppMonth year @ (Year y) month @ (Month m) Calendar { .. } =
    between "\n" (months !! (m - 1) ++ " " ++ show y)
    ++ "\n" ++ row width (map rightAligned weekDays)
    ++ concatMap oneWeek [0 .. weeks]
    where
        weeks = if monthLength' == 28 && start == 0 then 3 else 4
        monthLength' = monthLength y m
        oneWeek n = week width 4 (map showDay (take 7 [1 + 7 * n - start..])) [[[]]]
        start = dayOfWeek $ Date year month (Day 1)
        showDay n
            | n > 0 && n <= monthLength' = show n
            | otherwise = ""

        events' :: [(VEvent, (Maybe Int, Maybe Int))]
        events' = mapMaybe (withinMonth year month) events

        sameDayEvents = sortBy (comparing (dtStart . fst)) 
            $ filter (\case 
                (_, (Just a, Just b)) -> a == b
                _ -> False) events'

        -- sortBy (comparing (dtStart . fst)) 

        showEvent (Just a, Just b) = a 

        width = 16

ppTime :: Time -> String
ppTime (Time (Hour h) (Minute m) _) =
    show h ++ ":" ++ show m

withinMonth :: Year -> Month -> VEvent -> Maybe (VEvent, (Maybe Int, Maybe Int))
withinMonth y m event @ VEvent { .. }
    | startAndEnd == (Nothing, Nothing) = Nothing 
    | otherwise = Just (event, startAndEnd)
    where
        startAndEnd = (inMonth dtStart, inMonth dtEnd)
        inMonth = dayInMonth y m . date

dayInMonth :: Year -> Month -> Date -> Maybe Int
dayInMonth y m (Date year month (Day d))
    | y == year && m == month = Just d
    | otherwise = Nothing

week :: Int -> Int -> [String] -> [[String]] -> String
week width height names events = divider (width + 2) 7
    ++ row' names
   -- ++ (concat $ map row' (map (padded "" 7) $ map leftAligned events))
    where
        row' = row width . map leftAligned
        height' = maximum $ map length events
        emptyRow = row width $ replicate 7 emptyString

padded :: a -> Int -> ([a], [a]) -> [a]
padded a n (l, r) = l ++ replicate (n - length l - length r) a ++ r

leftAligned :: Monoid a => a -> (a, a)
leftAligned s = (s, mempty)

rightAligned :: Monoid a => a -> (a, a)
rightAligned s = (mempty, s)

emptyString :: Monoid a => (a, a)
emptyString = (mempty, mempty)

row :: Int -> [(String, String)] -> String
row width = (++ "\n")
    . intercalate "|" 
    . map (between " " . padded ' ' width)

divider :: Int -> Int -> String
divider width length = (++ "\n")
    . intercalate "+" 
    $ replicate length 
    $ replicate width '-'

between :: [a] -> [a] -> [a]
between x y = x ++ y ++ x

weekDays :: [String]
weekDays =
    [ "Monday"
    , "Tuesday"
    , "Wednesday"
    , "Thursday"
    , "Friday"
    , "Saturday"
    , "Sunday"
    ]

months :: [String]
months = 
    [ "January"
    , "February"
    , "March"
    , "April"
    , "May"
    , "June"
    , "July"
    , "August"
    , "September"
    , "October"
    , "November"
    , "December"
    ]

dayOfWeek :: Date -> Int
dayOfWeek (Date (Year y) (Month m) (Day d)) =
    ( y' + y' // 4 - y' // 100 + y' // 400 + monthValue !! (m - 1) + d - 1) `mod` 7
    where
        (//) = div
        y' = y - if m < 3 then 1 else 0
        monthValue = [0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4]
