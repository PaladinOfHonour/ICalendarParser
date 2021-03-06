{-# LANGUAGE TemplateHaskell #-}
module Main where

-- | TemplateHaskell used by DeriveTH

-- !! The program only compiles with "ghc --make -O YourFile.hs" if the pacakges below are acccesible, beware of sandbox etc !!

import Prelude hiding ((<*>),(*>),(<*),(<$>),(<$),($>))
import Data.List (intercalate, find, permutations, concatMap, unzip, sort, nubBy)
import Data.Text (stripEnd, pack, unpack)
import Data.Maybe (fromJust, isNothing)

import ParseLib.Abstract hiding (pack)
import System.Environment
import System.IO
import qualified Data.Time.Calendar as C (fromGregorian, diffDays, Day(..))
import Data.DeriveTH (derive, makeIs)

-- Starting Framework

                -- |   Time related Datatypes | --

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc :: Bool }
    deriving (Eq, Ord)

instance Show DateTime where
    show = printDateTime

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

-- | Show instance implementation, it being a seperate function is superfluous
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

-- | Represents all the primary elements of our grammar
data Token = 
    BEv
  | EEv
  | Stamp DateTime
  | UID String
  | DTS DateTime
  | DTE DateTime
  | DES String
  | SUM String
  | LOC String
  | BCal
  | ECal
  | ProdID String
  | Version String
  deriving (Eq, Ord, Show) --{-! Is !-}

--------- Splice -----------
--Above this point the derived makeIs functions oughtn't be applied
--------- Splice -----------
-- Derive magic
$( derive makeIs ''Token )

-- | The main interaction function. Used for IO, do not edit.
data Result = SyntaxError | Invalid DateTime | Valid DateTime deriving (Eq, Ord)

instance Show Result where
    show SyntaxError = "date/time with wrong syntax"
    show (Invalid _) = "good syntax, but invalid date or time values"
    show (Valid x)   = "valid date: " ++ show x

-- | Main loop, as we haven't implemented ex 11 it is a workig copy of "mainCalendar" [Without the pretty print]
main :: IO ()
main = do
    file:_  <- getArgs
    res     <- readCalendar file
    putStrLn $ show res
    putStrLn $ maybe "Calendar parsing error" (show) res

-- | Used to parse just DateTimes
mainDateTime :: IO ()
mainDateTime = interact (printOutput . processCheck . processInput)
    where
        processInput = map (run parseDateTime) . lines
        processCheck = map (maybe SyntaxError (\x -> if checkDateTime x then Valid x else Invalid x))
        printOutput  = unlines . map show

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = (\x _ y z -> DateTime x y z) <$> dateP <*> anySymbol <*> timeP <*> utcP 

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p x =   let res = parse p x
            in  case res of
                    []  -> Nothing
                    _   -> case head res of 
                            (xs,[]) -> Just xs
                            _       -> Nothing

-- Exercise 3
--Due to the splice, this has been moved to line 41

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

-- | checks for a leap year
leapYear :: Int -> Bool
leapYear x      | mod x 400 == 0 = True
                | mod x 100 == 0 = False
                | mod x 4 == 0 = True
                | otherwise = False
    
 -- | Takes bool for leapyear and an int for month/day
checkDay :: Bool -> Int -> Int -> Bool
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
    deriving (Eq, Ord)
-- | Custom print for the Calendar
instance Show Calendar where
    show Calendar{prodid = id, version = ver, event = evs} =
               "BEGIN:VCALENDAR\r\n"
            ++ "PRODUCT_ID:" ++ id ++ "\r\n"
            ++ "VERSION:" ++ ver ++ "\r\n"
            ++ (concatMap showEv evs)
            ++ "END:VCALENDAR"
        where
            showEv Event{dtstamp = stamp, uid = uniqId, dtstart = dtS, dtend = dtE, description = des, summary = sum, location = loc} =
                       "BEGIN:VEVENT\r\n"
                    ++ "UID:" ++ uniqId ++ "\r\n"
                    ++ "DTSTAMP:" ++ (printDateTime stamp) ++ "\r\n"
                    ++ "DTSTART:" ++ (printDateTime dtS) ++ "\r\n"
                    ++ "DTEND:" ++ (printDateTime dtE) ++ "\r\n"
                    ++ (concatMap checkEmpty [(des,"DESCRIPTION:"), (sum,"SUMMARY:"), (loc,"LOCATION:")])
                    ++ "END:VEVENT\r\n"
            checkEmpty (Nothing, _) = ""
            checkEmpty (Just v,s)   = s ++ v ++ "\r\n"

-- | Due to the use of listOf as our main Lexing method, the Parser crashes if the input ends on a "\n\r"; this removes that
cheekyCheck :: String -> String
cheekyCheck  = reverse . f . reverse
                where f ('\n':'\r':xs) = xs
                      f s              = s

data Event = Event  { dtstamp :: DateTime 
                    , uid :: String
                    , dtstart :: DateTime
                    , dtend :: DateTime
                    , description :: Maybe String
                    , summary :: Maybe String
                    , location :: Maybe String}
    deriving (Eq, Ord, Show)

-- Exercise 7
scanCalendar :: Parser Char [Token]
scanCalendar =  flip listOf pSep
                (
                (*>) pSpaces $ foldl (<|>) pBeginCalHeader
                 [
                     pBeginEvHeader, pEndCalHeader, pEndEvHeader,
                     pProdID, pVersion,
                     pUID, pDTStamp, pDateTimeStart, pDateTimeEnd, pDescription, pSummary, pLocation
                 ]
                 )
                <* eof

pSep :: Parser Char Bool
pSep = True <$ token "\r\n"


parseCalendar :: Parser Token Calendar
parseCalendar = parseCalendar1 <|> parseCalendar2 -- parser either the version or the prodID first than goes on to parse events at line 407
parseCalendar1 = (\x y z -> Calendar y x z) <$ parseBCal <*> parseVersion <*> parseProdID <*> parseEvents <* parseECal
parseCalendar2 = (\x y z -> Calendar x y z) <$ parseBCal <*> parseProdID <*> parseVersion <*> parseEvents <* parseECal

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar s = do
    h <- openFile s ReadMode
    hSetNewlineMode h noNewlineTranslation
    cont <- hGetContents h

    let cont' = cheekyCheck cont
    let res = recognizeCalendar cont'
    res `seq` (hClose h)
    return res

-- Exercise 9
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar = show    -- for implementation see instace Show of Calendar

-- Exercise 10
countEvents :: Calendar -> Int
countEvents = length . event

findEvents :: DateTime -> Calendar -> [Event]
findEvents t =  let res = filter (\e -> (dtstart e >= t) && (t <= dtend e) ) . event
                in res

checkOverlapping :: Calendar -> Bool
checkOverlapping = ((/=0) . length) . nubBy check_overlap . (\(x,y) -> zip x y) . (\(a,b) -> (sort a, sort b)) . unzip . map (\e -> (dtstart e, dtend e)) . event
    where check_overlap (_,b) (c,_) = c < b

-- | Calculates the amount of time spent on activities with a given summary
-- | If there's overlap, the function isn't defined
timeSpent :: String -> Calendar -> Int
timeSpent s c = res c
        where
            res         = count . map (\e -> (dtstart e, dtend e)) . filter ((s==) . fromJust . summary) . event
            count       = case checkOverlapping c of
                            False   -> sum . map (\(a,b) -> a -$- b ) 
                            True    -> undefined --sum . (\z@(a,b) v@(c,d) -> if c < b then  else z -$- v )  . (\(x,y) -> zip x y) . (\(a,b) -> (sort a, sort b)) . unzip

-- | a difference operator for two DateTimes, returns the difference in minutes
(-$-) :: DateTime -> DateTime -> Int
(-$-) DateTime{ date = date1, time = time1 } DateTime{ date = date2, time = time2 }
        = (fromInteger diffD) * 1440 + (h2 - h1) * 60 + (m2 - m1) + (s2 - s1) `div` 60
    where
            diffD       = C.diffDays (C.fromGregorian (toInteger y1) m1 d1) (C.fromGregorian (toInteger y2) m2 d2)
            y1      = unYear    $   year   date1
            m1      = unMonth   $   month  date1
            d1      = unDay     $   day    date1
            h1      = unHour    $   hour   time1
            min1    = unMinute  $   minute time2
            s1      = unSecond  $   second time2
            y2      = unYear    $   year   date2
            m2      = unMonth   $   month  date2
            d2      = unDay     $   day    date2
            h2      = unHour    $   hour   time2
            min2    = unMinute  $   minute time2
            s2      = unSecond  $   second time2

-- Exercise 11
ppMonth :: Year -> Month -> Calendar -> String
ppMonth = undefined

-- help parsers
-- Time Related

dateP :: Parser Char Date
dateP = Date <$> yearP <*> monthP <*> dayP

yearP :: Parser Char Year 
yearP = (\x y z w -> Year $ read $ [x,y,z,w]) <$> digit <*> digit <*> digit <*> digit

monthP :: Parser Char Month 
monthP = (\x y -> Month $ read $ [x,y]) <$> digit <*> digit

dayP :: Parser Char Day
dayP = (\x y -> Day $ read $ [x,y]) <$> digit <*> digit

timeP :: Parser Char Time
timeP = Time <$> hourP <*> minuteP <*> secondP

hourP :: Parser Char Hour 
hourP = (\x y -> Hour $ read $ [x,y]) <$> digit <*> digit

minuteP :: Parser Char Minute
minuteP = (\x y -> Minute $ read $ [x,y]) <$> digit <*> digit

secondP :: Parser Char Second
secondP = (\x y -> Second $ read $ [x,y]) <$> digit <*> digit

utcP :: Parser Char Bool
utcP = (\x -> x == 'Z') <$> option anySymbol 'F'

-- Token Related
pBeginCalHeader :: Parser Char Token
pBeginCalHeader = BCal <$ token "BEGIN:VCALENDAR" <* pSpaces

pBeginEvHeader :: Parser Char Token
pBeginEvHeader  = BEv <$ token "BEGIN:VEVENT" <* pSpaces 

pEndCalHeader :: Parser Char Token
pEndCalHeader   = ECal <$ token "END:VCALENDAR" <* pSpaces

pEndEvHeader :: Parser Char Token
pEndEvHeader    = EEv <$ token "END:VEVENT" <* pSpaces

pProdID :: Parser Char Token
pProdID         = (<$>) ProdID $ token "PRODID:" *> pString_ <* pSpaces

pVersion :: Parser Char Token
pVersion        = (<$>) Version $ token "VERSION:" *> pString_ <* pSpaces

pUID :: Parser Char Token
pUID            = (<$>) UID $ token "UID:" *> pString_ <* pSpaces

pDTStamp :: Parser Char Token
pDTStamp        = (<$>) Stamp $ token "DTSTAMP:" *> parseDateTime <* pSpaces

pDateTimeStart :: Parser Char Token
pDateTimeStart  = (<$>) DTS $ token "DTSTART:" *> parseDateTime <* pSpaces

pDateTimeEnd :: Parser Char Token
pDateTimeEnd    = (<$>) DTE $ token "DTEND:" *> parseDateTime <* pSpaces

pDescription :: Parser Char Token
pDescription    = (<$>) DES $ token "DESCRIPTION:" *> pString_ <* pSpaces

pSummary :: Parser Char Token
pSummary        = (<$>) SUM $ token "SUMMARY:" *> pString_ <* pSpaces

pLocation :: Parser Char Token
pLocation       = (<$>) LOC $ token "LOCATION:" *> pString_ <* pSpaces

pString_ :: Parser Char String
pString_ = some $ satisfy (/= '\r')

pSpaces :: Parser Char String
pSpaces         = many $ satisfy (\c -> c == ' ')


parseBCal :: Parser Token Token
parseBCal = satisfy (\x -> x == BCal)

parseECal :: Parser Token Token
parseECal = satisfy (\x -> x == ECal)

parseVersion :: Parser Token String
parseVersion = whatVersion <$> satisfy isVersion

isVersion2 :: Token -> Bool
isVersion2 (Version _) = True
isVersion2 _        = False

whatVersion :: Token -> String
whatVersion (Version x) = x 
whatVersion _        = error "This is not the version you were looking for."

parseProdID :: Parser Token String
parseProdID = whatProdID <$> satisfy isProdID2

isProdID2 :: Token -> Bool
isProdID2 (ProdID _) = True
isProdID2 _          = False

whatProdID :: Token -> String
whatProdID (ProdID x) = x 
whatProdID _          = error "This is not the prodID you were looking for."

parseEvents :: Parser Token [Event]
parseEvents = many parseEvent <|> const [] <$> parseECal

parseEvent :: Parser Token Event
parseEvent = fromJust . eventTokenHandler . permutations <$ parseBEvent <*> greedy parseAllEvent <* parseEEvent

eventTokenHandler :: [[Token]] -> Maybe Event   -- Recursively go through all permutations
eventTokenHandler [] = Nothing                  -- pattern match on the tokens for every case
eventTokenHandler [[]] = Nothing
eventTokenHandler a = case head a of
                        [Stamp x,UID y,DTS z,DTE r]                     -> Just (Event x y z r Nothing Nothing Nothing)
                        [Stamp x,UID y,DTS z,DTE r,DES s]               -> Just (Event x y z r (Just s) Nothing Nothing)
                        [Stamp x,UID y,DTS z,DTE r,SUM t]               -> Just (Event x y z r Nothing (Just t) Nothing)
                        [Stamp x,UID y,DTS z,DTE r,LOC u]               -> Just (Event x y z r Nothing Nothing (Just u))
                        [Stamp x,UID y,DTS z,DTE r,DES s,SUM t]         -> Just (Event x y z r (Just s) (Just t) Nothing)
                        [Stamp x,UID y,DTS z,DTE r,DES s,LOC u]         -> Just (Event x y z r (Just s) Nothing (Just u))
                        [Stamp x,UID y,DTS z,DTE r,SUM t,LOC u]         -> Just (Event x y z r Nothing (Just t) (Just u))
                        [Stamp x,UID y,DTS z,DTE r,DES s,SUM t,LOC u]   -> Just (Event x y z r (Just s) (Just t) (Just u))
                        otherwise                                       -> eventTokenHandler (tail a)


parseBEvent :: Parser Token Token
parseBEvent = satisfy (\x -> x == BEv)

parseEEvent :: Parser Token Token
parseEEvent = satisfy (\x -> x == EEv)

parseAllEvent :: Parser Token Token
parseAllEvent = satisfy isStamp <|> satisfy isUID <|> satisfy isDTS <|> satisfy isDTE <|> satisfy isDES <|> satisfy isSUM <|> satisfy isLOC

-- Legacy Code

-- No easy way to automatically generate a list of all the derived functions
{-
mandatoryTokens :: [Token] -> Bool
mandatoryTokens = (one_s==) . flip map (count `fmap` is_s) . flip ($)
            where
                count f = length . filter f
                is_s  = [isBEv, isECal, isProdID, isVersion]
                one_s = replicate (length is_s)  1

tokenRestraints :: [Token] -> Bool
tokenRestraints = flip map (count `fmap` is_s) . flip ($)
    where count f = length . filter f
          is_s  = [isBEv, isEEv, isStamp, isUID, isDTS, isDTE, isDES, isSUM, isLOC, isBCal, isECal, isProdID, isVersion]


setupCalendar :: [Token] -> Maybe Calendar
setupCalendar tks = let events = []
    in do
        let pId = find isProdID tks
        let v   = find isVersion tks
        case (pId, v) of
            (Nothing, _)                                -> Nothing
            (_, Nothing)                                -> Nothing
            ( Just (ProdID pId_), Just (Version v_) )   -> Just Calendar {prodid= pId_, version = v_, event=events}
-}