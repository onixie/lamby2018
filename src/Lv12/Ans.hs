{-# LANGUAGE DeriveFunctor #-}
module Lv12.Ans (answerOfLv12Part1, answerOfLv12Part2) where

import Text.ParserCombinators.Parsec (ParseError, parse, sepEndBy, between, count, GenParser, string, many, many1, choice, char)
import Text.Parsec.Char (spaces, newline)
import Control.Arrow (right)
import Data.List (find)
import Data.Array (Array)

type Pot = Char

data Caven p = Caven { pots :: p, notes :: [([Pot], Pot)] } deriving (Show, Functor)

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
  return $ Caven is (filter (('#'==) . snd) ns)

readCaven :: FilePath -> IO (Either ParseError (Caven [Pot]))
readCaven filePath = readFile filePath >>= return . parse caven filePath

generate 0 caven = caven
generate n caven@(Caven indexedPots notes) = generate (n-1) $ caven { pots = trimming $ nextGen padding notes }
  where
    nextGen []          notes = []
    nextGen indexedPots notes = match (take 5 indexedPots) notes : nextGen (drop 1 indexedPots) notes
    match pots notes = let ci = 2 + (fst $ head pots)
                           pts = map snd pots in
                         case {-# SCC findNote #-} find ((pts ==) . fst) notes of
                           Just (pots, pot) -> (ci, pot)
                           _ -> (ci, '.')
    padding = let start = fst $ head indexedPots
                  end   = fst $ last indexedPots in 
                zip [start - 4 .. start - 1] "...." ++ indexedPots ++ zip [end+1..end+4] "...."
    trimming = reverse . dropWhile ((/= '#') . snd) . reverse . dropWhile ((/= '#') . snd) 


answerOfLv12Part1 = readCaven "input.txt" 
  >>= return . right (sum . map fst . filter ((=='#') . snd) . pots . generate 20 . fmap (zip [0..]))

answerOfLv12Part2 filePath g = readCaven filePath
  >>= return . right (sum . map fst . filter ((=='#') . snd) . pots . generate g . fmap (zip [0..]))

