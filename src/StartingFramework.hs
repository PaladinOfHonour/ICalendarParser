module Main where

import ParseLib.Abstract
import System.Environment

-- Starting Framework

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord)

newtype Year  = Year { unYear :: Int }  deriving (Eq, Ord)
newtype Month = Month { unMonth :: Int } deriving (Eq, Ord)
newtype Day   = Day { unDay :: Int } deriving (Eq, Ord)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord)

newtype Hour   = Hour { unHour :: Int } deriving (Eq, Ord)
newtype Minute = Minute { unMinute :: Int } deriving (Eq, Ord)
newtype Second = Second { unSecond :: Int } deriving (Eq, Ord)


-- | The main interaction function. Used for IO, do not edit.
data Result = SyntaxError | Invalid DateTime | Valid DateTime deriving (Eq, Ord)

instance Show DateTime where
    show = printDateTime

instance Show Result where
    show SyntaxError = "date/time with wrong syntax"
    show (Invalid _) = "good syntax, but invalid date or time values"
    show (Valid x)   = "valid date: " ++ show x

main :: IO ()
main = mainDateTime

mainDateTime :: IO ()
mainDateTime = interact (printOutput . processCheck . processInput)
    where
        processInput = map (run parseDateTime) . lines
        processCheck = map (maybe SyntaxError (\x -> if checkDateTime x then Valid x else Invalid x))
        printOutput  = unlines . map show

mainCalendar :: IO ()
mainCalendar = do
    file:_ <- getArgs
    res <- readCalendar file
    putStrLn $ maybe "Calendar parsing error" (ppMonth (Year 2012) (Month 11)) res

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = (\x _ y z -> DateTime x y z) <$> dateP <*> anySymbol <*> timeP <*> utcP 

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p x = case head (parse p x) of 
        (xs,[]) -> Just xs
        _       -> Nothing

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime x = yearx ++ monthx ++ dayx ++ ['T'] ++ hourx ++ minutex ++ secondx ++ utcx
                where 
                    yearx = show $ unYear $ year $ date x
                    monthx = show $ unMonth $ month $ date x
                    dayx = show $ unDay $ day $ date x
                    hourx = show $ unHour $ hour $ time x
                    minutex = show $ unMinute $ minute $ time x
                    secondx = show $ unSecond $ second $ time x
                    utcx = case utc x of
                            True -> ['Z']
                            False -> []


-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime x     | monthBool && dayBool && hourBool && minuteBool && secondBool = True
                    | otherwise = False
                where 
                    jaar = unYear $ year $ date x
                    maand = unMonth $ month $ date x
                    dag = unDay $ day $ date x
                    leap = leapYear $ jaar
                    monthBool = checkMonth $ maand
                    dayBool = checkDay leap maand dag
                    hourBool = checkHour $ unHour $ hour $ time x
                    minuteBool = checkMinute $ unMinute $ minute $ time x
                    secondBool = checkSecond $ unSecond $ second $ time x



-- help functions
checkMonth :: Int -> Bool
checkMonth x  | x < 13 && x > 0   = True
              | otherwise         = False

leapYear :: Int -> Bool
leapYear x      | mod x 400 == 0 = True
                | mod x 100 == 0 = False
                | mod x 4 == 0 = True
                | otherwise = False

checkDay :: Bool -> Int -> Int -> Bool -- Takes bool for leapyear and an int for month/day
checkDay b m d  | d < 32 && d > 0 && 
                (m == 1 || m == 3 || m == 5 || m == 7 || 
                m ==  8 || m == 10 || m == 12)                  = True
                | d < 31 && d > 0 && 
                (m == 4 || m == 6 || m == 9 || m == 11)         = True
                | d < 30 && d > 0 && m == 2 && b == True        = True
                | d < 29 && d > 0 && m == 2 && b == False       = True
                | otherwise                                     = False
              
checkHour :: Int -> Bool
checkHour x | x <= 23 && x >= 0 = True
            | otherwise = False

checkMinute :: Int -> Bool
checkMinute x   | x <= 59 && x >= 0 = True
                | otherwise = False    

checkSecond :: Int -> Bool
checkSecond x   | x <= 59 && x >= 0 = True
                | otherwise = False



-- Exercise 6
data Calendar = Calendar { prodid :: String
                         , version :: String
                         , event :: [Event] }
    deriving (Eq, Ord, Show)


data Event = Event  { dtstamp :: DateTime 
                    , uid :: String
                    , dtstart :: DateTime
                    , dtend :: DateTime
                    , description :: String
                    , summary :: String
                    , location :: String}
    deriving (Eq, Ord, Show)

-- Exercise 7
data Token = Token  
    deriving (Eq, Ord, Show)

scanCalendar :: Parser Char [Token]
scanCalendar = undefined

parseCalendar :: Parser Token Calendar
parseCalendar = undefined

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar = undefined

-- Exercise 9
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar = undefined

-- Exercise 10
countEvents :: Calendar -> Int
countEvents = undefined

findEvents :: DateTime -> Calendar -> [Event]
findEvents = undefined

checkOverlapping :: Calendar -> Bool
checkOverlapping = undefined

timeSpent :: String -> Calendar -> Int
timeSpent = undefined

-- Exercise 11
ppMonth :: Year -> Month -> Calendar -> String
ppMonth = undefined

-- help parsers
dateP :: Parser Char Date
dateP = (\x y z -> Date x y z) <$> yearP <*> monthP <*> dayP

yearP :: Parser Char Year 
yearP = (\x y z w -> Year $ read $ [x,y,z,w]) <$> digit <*> digit <*> digit <*> digit

monthP :: Parser Char Month 
monthP = (\x y -> Month $ read $ [x,y]) <$> digit <*> digit

dayP :: Parser Char Day
dayP = (\x y -> Day $ read $ [x,y]) <$> digit <*> digit

timeP :: Parser Char Time
timeP = (\x y z -> Time x y z) <$> hourP <*> minuteP <*> secondP

hourP :: Parser Char Hour 
hourP = (\x y -> Hour $ read $ [x,y]) <$> digit <*> digit

minuteP :: Parser Char Minute
minuteP = (\x y -> Minute $ read $ [x,y]) <$> digit <*> digit

secondP :: Parser Char Second
secondP = (\x y -> Second $ read $ [x,y]) <$> digit <*> digit

utcP :: Parser Char Bool
utcP = (\x -> x == 'Z') <$> option anySymbol 'F'

