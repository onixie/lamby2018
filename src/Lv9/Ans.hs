module Lv9.Ans (answerOfLv9Part1, answerOfLv9Part2, answerOfLv9Part) where

import Text.ParserCombinators.ReadP (char, sepBy, endBy, ReadP, many1, satisfy, string, readP_to_S)
import Data.Char (isDigit)
import Data.List (maximumBy)
import Data.Array (Array, array, (//), (!))
import Data.Maybe (isJust, fromJust)

type Id = Int
type Score = Int
type Marble = Int
data Player = Player { pid :: Id, score :: Score } deriving ( Show )
data Circle = Circle { current :: Int, marbles :: [Marble], removed :: Maybe Marble } deriving ( Show )
data MarbleGame = MkGame { players :: Array Int Player, circle :: Circle, nextMarble :: Marble, lastMarble :: Marble, numOfPlayers :: Int } deriving ( Show )

loadGame filePath = readFile filePath >>= return . fst . (!!0) . readP_to_S readGame

loadGames filePath = readFile filePath >>= return . fst . last . readP_to_S (readGame `sepBy` char '\n')

readGame :: ReadP MarbleGame
readGame = do 
  numOfPlayers <- number >>= return . read
  string " players; last marble is worth "
  lastMarble <- number >>= return . read
  string " points"
  return $ gameWith numOfPlayers lastMarble
  where
    number = many1 (satisfy isDigit)

gameWith numOfPlayers lastMarble = MkGame (array (1, numOfPlayers) [(id, Player id 0) | id <- [1..numOfPlayers]]) (Circle 0 [0] Nothing) 1 lastMarble numOfPlayers

play :: MarbleGame -> MarbleGame
play game@(MkGame players circle nextMarble lastMarble _) | nextMarble > lastMarble = game
                                                          | otherwise = play $ (updateRound (updatePlayer (updateCircle game)))

updateRound game = game { nextMarble = nextMarble game + 1 }

updatePlayer game@(MkGame players circle@(Circle current marbles extraScore) nextMarble _ _)
  | nextMarble `rem` 23 == 0 && isJust extraScore = game { players = players // [(pid nextPlayer, nextPlayer{score=score nextPlayer + nextMarble + fromJust extraScore})] }
  | otherwise = game
  where
    nextPlayer = players ! (((nextMarble - 1) `rem` numOfPlayers game) + 1)

updateCircle game@(MkGame _ circle@(Circle current marbles _) nextMarble _ _)
  | nextMarble `rem` 23 == 0 = let next = goCounterClockWise (length marbles) current 7
                                   succNext = goClockWise (length marbles) next 1 in
                                 game { circle = {-# SCC updateCircle23 #-} Circle (if succNext == 0 then succNext else next) (take next marbles ++ drop (next+1) marbles) (Just (marbles!!next))}
  | otherwise = let next = goClockWise (length marbles) current 1 + 1 in
                  game { circle = {-# SCC updateCircle0 #-} Circle next (take next marbles ++ [nextMarble] ++ drop next marbles) Nothing }

goClockWise count current n = (current + n) `rem` count

goCounterClockWise count current n = let res = current - n in
                                       if res > 0
                                       then res
                                       else if res `rem` count == 0
                                       then 0
                                       else count + res `rem` count

answerOfLv9Part1 = answerOfLv9Part "input.txt" 1
answerOfLv9Part2 = answerOfLv9Part "input.txt" 2
answerOfLv9Part filePath n = (loadGames filePath) >>= return . maximumBy ((.score) . compare . score) . players . play . (!!(n-1))
