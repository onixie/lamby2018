module Lv4.Ans ( answerOfLv4Part1
               , answerOfLv4Part2
               ) where

import Data.Time (secondsToDiffTime, diffUTCTime,  addUTCTime, DiffTime,  Day
                 , UTCTime(..)
                 , defaultTimeLocale
                 , readPTime
                 , utctDay
                 , utctDayTime
                 , getCurrentTime
                 )

import Text.ParserCombinators.ReadP ( ReadP
                                    , ReadS
                                    , readP_to_S
                                    , many
                                    , many1
                                    , satisfy
                                    , between
                                    , choice
                                    , string
                                    , char
                                    , skipSpaces
                                    , endBy
                                    , eof
                                    )
import Data.Char ( isDigit
                 )

import Data.List (minimumBy, maximumBy, sort, partition, groupBy,  sortOn
                 )

import Control.Exception

type GuardId = Int

data Observed = BeginShift GuardId | WakeUp | FallAsleep deriving (Show, Eq)

data Record = Record { event :: Observed
                     , at :: UTCTime
                     } deriving (Show, Eq)

loadRecordsChronologically :: IO [Record]
loadRecordsChronologically = readFile "input.txt" >>= return . fst . (!!0) . readRecordsChronologically

readRecordsChronologically :: ReadS [Record]
readRecordsChronologically = readP_to_S $ (record `endBy` newline <* eof) >>= return . sortOn at
  where 
    record :: ReadP Record
    record = do ts <- between (char '[') (char ']') timestamp
                e <- skipSpaces *> event
                return $ Record e ts

    timestamp :: ReadP UTCTime
    timestamp = readPTime False defaultTimeLocale "%F %H:%M"

    event :: ReadP Observed
    event = choice [ string "wakes up" *> return WakeUp
                   , string "falls asleep" *> return FallAsleep
                   , between (string "Guard #") (string " begins shift") (many1 (satisfy isDigit) >>= return . read) >>= return . BeginShift
                   ]
    newline = char '\n'

data Daily = Daily { guard :: GuardId
                   , day   :: Day
                   , awake  :: [(UTCTime, UTCTime)]
                   , asleep :: [(UTCTime, UTCTime)]
                   } deriving (Show, Eq)

daily :: [Record] -> [Daily]
daily records = toDaily records Nothing []
  where
    -- start of new daily: shift -> sleep or end of the day
    toDaily ((Record (BeginShift id) beginShift):rs@((Record FallAsleep beginAsleep):_)) Nothing ds =
      toDaily rs (Just $ Daily { guard = id, day = utctDay beginShift, awake = [(beginShift, beginAsleep)], asleep = [] }) ds

    toDaily ((Record (BeginShift id) beginShift):rs@((Record (BeginShift _) endOfDuty):_)) Nothing ds =
      toDaily rs Nothing ( ds ++ [ Daily { guard = id, day = utctDay beginShift, awake = [(beginShift, endOfDuty)], asleep = [] } ] )

    -- in the daily: wakeup -> sleep or end of the day or records
    toDaily ((Record WakeUp beginAwake):rs@((Record FallAsleep beginAsleep):_)) (Just d) ds = 
      toDaily rs (Just $ d { awake = awake d ++ [(beginAwake, beginAsleep)]}) ds

    toDaily ((Record WakeUp beginAwake):rs@((Record (BeginShift id) endOfDuty):_)) (Just d) ds = 
      toDaily rs Nothing ( ds ++ [ d { awake = awake d ++ [(beginAwake, endOfDuty)]} ] )

    toDaily ((Record WakeUp beginAwake):[]) (Just d) ds = 
      ds ++ [ d { awake = awake d ++ [(beginAwake, UTCTime (utctDay beginAwake) anHour)]} ]

    -- in the daily: sleep -> wakeup or end of the day or records
    toDaily ((Record FallAsleep beginAsleep):rs@((Record WakeUp beginAwake):_)) (Just d) ds = 
      toDaily rs (Just $ d { asleep = asleep d ++ [(beginAsleep, beginAwake)]}) ds

    toDaily ((Record FallAsleep beginAsleep):rs@((Record (BeginShift _) endOfDuty):_)) (Just d) ds =
      toDaily rs Nothing (ds ++ [d { asleep = asleep d ++ [(beginAsleep, endOfDuty)]}])

    toDaily ((Record FallAsleep beginAsleep):[]) (Just d) ds =
      ds ++ [d { asleep = asleep d ++ [(beginAsleep, UTCTime (utctDay beginAsleep) anHour)]}]

    anHour = 3600

total = (+)
timeOf f acc daily = foldl f 0 $ map (\s -> (diffUTCTime (snd s) (fst s)) / 60) (acc daily)
timeRangeByMinutes (from, to) = [t | t <- from:if from < to then timeRangeByMinutes(addUTCTime 60 from, to) else [], from < to]

mostFreqMinutes :: [Daily] -> (GuardId, [UTCTime]) -- assume: all daily records are grouped by guard id
mostFreqMinutes group =
  let g = guard $ group!!0
      allSleeps = foldl (++) [] $ map asleep group
      sleepsGroupedByMin = groupBy (\x y -> utctDayTime x == utctDayTime y) . sortOn utctDayTime $ foldr (\r acc-> timeRangeByMinutes r ++ acc) [] allSleeps
      s = if sleepsGroupedByMin == [] then [] else maximumBy (\x y -> length x `compare` length y) sleepsGroupedByMin in
    assert (all (\(from, to) -> utctDay from == utctDay to && utctDayTime from < utctDayTime to) allSleeps) (g, s)
  
answerOfLv4Part1 = do 
  dailyRecords <- loadRecordsChronologically >>= return . daily
  let groupedDailyRecords = groupBy (\x y -> guard x == guard y) $ sortOn guard dailyRecords
      candidateGroup = (reverse $ sortOn (\r -> foldl (+) 0 $ map (timeOf total asleep) r) groupedDailyRecords)!!0
      (g, s) = mostFreqMinutes candidateGroup
  return . assert ( s/=[] ) $ (utctDayTime (s!!0)) / 60 * secondsToDiffTime (toInteger g)

answerOfLv4Part2 = do
  dailyRecords <- loadRecordsChronologically >>= return . daily
  let groupedDailyRecords = groupBy (\ x y -> guard x == guard y) $ sortOn guard dailyRecords
  -- mapM_ (print . mostFreqMinutes ) groupedDailyRecords
      (g, s) = mostFreqMinutes $ maximumBy (\x y -> (length . snd $ mostFreqMinutes x) `compare` (length . snd $ mostFreqMinutes y)) groupedDailyRecords
  return . assert ( s/=[] ) $ (utctDayTime (s!!0)) / 60 * secondsToDiffTime (toInteger g)
