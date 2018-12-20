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
                                    , many1
                                    , satisfy
                                    , between
                                    , choice
                                    , string
                                    , char
                                    , skipSpaces
                                    )
type GuardId = Int

data Observation = BeginShift GuardId | WakeUp | FallAsleep deriving (Show, Eq)

data Record = Record { event :: Observation
                     , at :: UTCTime
                     } deriving (Show, Eq)

readRecord :: ReadS Record
readRecord = readP_to_S record
  where 
    record :: ReadP Record
    record = do ts <- between (char '[') (char ']') timestamp
                e <- skipSpaces *> event
                return $ Record e ts

    timestamp :: ReadP UTCTime
    timestamp = readPTime False defaultTimeLocale "%F %H:%M"

    event :: ReadP Observation
    event = choice [ string "wakes up" *> return WakeUp
                   , string "falls asleep" *> return FallAsleep
                   -- , between (string "Guard #") (string " begins shift") (many1 (satisfy isDigit) >>= return . read)
                   ]
  
answerOfLv4Part1 = undefined
answerOfLv4Part2 = undefined
