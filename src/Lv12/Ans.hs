{-# LANGUAGE DeriveFunctor #-}

module Lv12.Ans (answerOfLv12Part1, answerOfLv12Part2, answerOfLv12Part) where

import Text.ParserCombinators.Parsec (ParseError, parse, sepEndBy, between, count, GenParser, string, many, many1, choice, char)
import Text.Parsec.Char (spaces, newline)
import Control.Arrow (right)
import Data.Map.Strict (Map, fromList, findWithDefault)
import Data.List (elemIndex)

type Pot = Char

data Caven p = Caven { start :: Int, pots :: p, notes :: Map [Pot] Pot } deriving (Show, Functor)

plant = char '#'
empty = char '.'
pot = choice [plant, empty]

initialState :: GenParser Char st [Pot]
initialState = string "initial state: " >> many1 pot

note :: GenParser Char st ([Pot], Pot)
note = do 
  llcrr <- count 5 pot
  between spaces spaces $ string "=>"
  n <- pot
  return (llcrr, n)

caven :: GenParser Char st (Caven [Pot])
caven = do
  is <- initialState
  many newline
  ns <- note `sepEndBy` newline
  return $ Caven 0 is (fromList (filter (('#'==) . snd) ns))

readCaven :: FilePath -> IO (Either ParseError (Caven [Pot]))
readCaven filePath = readFile filePath >>= return . parse caven filePath

generate 0 caven = caven
generate n caven@(Caven start indexedPots notes) = let end = start + length indexedPots
                                                       current = start - 2
                                                       padding = "...." ++ indexedPots ++ "...." 
                                                       nextGen = next current padding notes 
                                                       Just nextStart = fmap (current+) $ elemIndex '#' nextGen in 
                                                     generate (n-1) $ caven { start = nextStart, pots = trimming $ nextGen }
  where
    next ci []          notes = []
    next ci indexedPots notes = match ci (take 5 indexedPots) notes : next (ci + 1) (drop 1 indexedPots) notes
    match ci pots notes = {-# SCC findNote #-} findWithDefault '.' pots notes
    trimming = reverse . dropWhile ((/= '#')) . reverse . dropWhile ((/= '#'))

answerOfLv12Part filePath n = do 
  Right (Caven start pots _) <- readCaven filePath >>= return . right (generate n)
  let ps = zip [start..] pots
  return . sum . map fst $ filter ((=='#').snd) ps

answerOfLv12Part1 = answerOfLv12Part "input.txt" 20

answerOfLv12Part2 = answerOfLv12Part "input.txt" 500000

