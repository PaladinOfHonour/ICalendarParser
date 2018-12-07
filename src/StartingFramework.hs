{-# LANGUAGE TemplateHaskell #-}
module Main where

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

-- Derive magic
$( derive makeIs ''Token )

-- | The main interaction function. Used for IO, do not edit.
data Result = SyntaxError | Invalid DateTime | Valid DateTime deriving (Eq, Ord)

instance Show Result where
    show SyntaxError = "date/time with wrong syntax"
    show (Invalid _) = "good syntax, but invalid date or time values"
    show (Valid x)   = "valid date: " ++ show x

    -- Test Data
    --"BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//hacksw/handcal//NONSGML v1.0//EN\r\nBEGIN:VEVENT\r\nUID:19970610T172345Z-AF23B2@example.com\r\nDTSTAMP:19970610T172345Z\r\nDTSTART:19970714T170000Z\r\nDTEND:19970714T190000Z\r\nSUMMARY:Bastille Day Party\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nUID:19970610T172345Z-AF23B2@example.com\r\nDTSTAMP:19970610T172345Z\r\nDTSTART:19970714T180000Z\r\nDTEND:19970715T200000Z\r\nSUMMARY:Bastille Day Party\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"
    --[BEv, UID "19970610T172345Z-AF23B2@example.com", Stamp x, DTS x, DTE x]
    --where x = DateTime {date = Date{year=Year 1998,month= Month 10,day= Day 8},time = Time{hour= Hour 6, minute= Minute 34, second= Second 55},utc= True}
main :: IO ()
main = do
    file:_  <- getArgs
    res     <- readCalendar file
    putStrLn $ show res
    putStrLn $ maybe "Calendar parsing error" (show) res
{-
main = putStrLn $ show . setupCalendar . concat $ map (fromJust . run scanCalendar)
        [
            "BEGIN:VCALENDAR ",
            " PRODID:-//hacksw/handcal//NONSGML v1.0//EN",
            "   VERSION:       2.0",
            "BEGIN:VEVENT    ",
            "SUMMARY:Bastille Day Party    ",
            "UID: 19970610T172345Z-AF23B2@example.com",
            "DTSTAMP:19970610T172345Z",
            "DTSTART:19970714T170000Z  ",
            "    DTEND:19970715T040000Z",
            "END:VEVENT",
            " END:VCALENDAR"
        ]
-}

mainDateTime :: IO ()
mainDateTime = interact (printOutput . processCheck . processInput)
    where
        processInput = map (run parseDateTime) . lines
        processCheck = map (maybe SyntaxError (\x -> if checkDateTime x then Valid x else Invalid x))
        printOutput  = unlines . map show
{-
mainCalendar :: IO ()
mainCalendar = do
    file:_ <- getArgs
    res <- readCalendar file
    putStrLn $ maybe "Calendar parsing error" (ppMonth (Year 2012) (Month 11)) res
-} --TODO

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
    deriving (Eq, Ord)

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
 {-permutations
   subsequence
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
  | Version Float
-}

parseCalendar :: Parser Token Calendar
parseCalendar = parseCalendar1 <|> parseCalendar2
parseCalendar1 = (\x y z -> Calendar y x z) <$ parseBCal <*> parseVersion <*> parseProdID <*> parseEvents <* parseECal
parseCalendar2 = (\x y z -> Calendar x y z) <$ parseBCal <*> parseProdID <*> parseVersion <*> parseEvents <* parseECal

{-
parsen tot beginevent of endcalender
permutations over deze tokens

BCal -> ProdID / Version -> (BEv)* -> ECal
(\x y [z] -> Calendar x y [z]) z kan leeg zijn x en y liggend aan prodid positie
parseEvents :: Parser Token [Event]
-}
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

timeSpent :: String -> Calendar -> Int
timeSpent s c = res c
        where
            res         = count . map (\e -> (dtstart e, dtend e)) . filter ((s==) . fromJust . summary) . event
            count       = case checkOverlapping c of
                            False   -> sum . map (\(a,b) -> a -$- b ) 
                            True    -> sum . (\(a,b) (c,d) -> )  . (\(x,y) -> zip x y) . (\(a,b) -> (sort a, sort b)) . unzip

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
parseEvent = fromJust . eventTokenHandler . oi <$ parseBEvent <*> greedy parseAllEvent <* parseEEvent

oi :: [Token] -> [[Token]]
oi x = permutations x

eventTokenHandler :: [[Token]] -> Maybe Event
eventTokenHandler [] = Nothing 
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
{-
isStamp2 :: Token -> Bool
isStamp2 (Stamp _) = True
isStamp2 _         = False

isUID2 :: Token -> Bool
isUID2 (UID _) = True
isUID2 _         = False

isDTS2 :: Token -> Bool
isDTS2 (DTS _) = True
isDTS2 _         = False

isDTE2 :: Token -> Bool
isDTE2 (DTE _) = True
isDTE2 _         = False

isDES2 :: Token -> Bool
isDES2 (DES _) = True
isDES2 _         = False

isSUM2 :: Token -> Bool
isSUM2 (SUM _) = True
isSUM2 _         = False

isLOC2 :: Token -> Bool
isLOC2 (LOC _) = True
isLOC2 _         = False
-}
-- Legacy Code

-- No easy way to automatically generate a list of all the derived functions

mandatoryTokens :: [Token] -> Bool
mandatoryTokens = (one_s==) . flip map (count `fmap` is_s) . flip ($)
            where
                count f = length . filter f
                is_s  = [isBEv, isECal, isProdID, isVersion]
                one_s = replicate (length is_s)  1

{-
tokenRestraints :: [Token] -> Bool
tokenRestraints = flip map (count `fmap` is_s) . flip ($)
    where count f = length . filter f
          is_s  = [isBEv, isEEv, isStamp, isUID, isDTS, isDTE, isDES, isSUM, isLOC, isBCal, isECal, isProdID, isVersion]
-}

setupCalendar :: [Token] -> Maybe Calendar
setupCalendar tks = let events = []
    in do
        let pId = find isProdID tks
        let v   = find isVersion tks
        case (pId, v) of
            (Nothing, _)                                -> Nothing
            (_, Nothing)                                -> Nothing
            ( Just (ProdID pId_), Just (Version v_) )   -> Just Calendar {prodid= pId_, version = v_, event=events}
