module Lv4.Ans ( answerOfLv4Part1
               , answerOfLv4Part2
               ) where

import Data.Time ( Day
                 , UTCTime
                 , defaultTimeLocale
                 , readPTime
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

import Data.List ( sortOn
                 )

type GuardId = Int

data Observed = BeginShift GuardId | WakeUp | FallAsleep deriving (Show, Eq)

data Record = Record { event :: Observed
                     , at :: UTCTime
                     } deriving (Show, Eq)

loadRecords :: IO [Record]
loadRecords = readFile "input.txt" >>= return . fst . (!!0) . readRecords

readRecords :: ReadS [Record]
readRecords = readP_to_S $ (record `endBy` newline <* eof) >>= return . sortOn at
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

answerOfLv4Part1 = undefined
answerOfLv4Part2 = undefined
