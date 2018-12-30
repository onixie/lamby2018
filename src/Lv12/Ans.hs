{-# LANGUAGE DeriveFunctor #-}
module Lv12.Ans (answerOfLv12Part1, answerOfLv12Part, answerOfLv12Part2) where

import Text.ParserCombinators.Parsec (ParseError, parse, sepEndBy, between, count, GenParser, string, many, many1, choice, char)
import Text.Parsec.Char (spaces, newline)
import Control.Arrow (right)
import Data.List (find)
import Data.Array (Array)
import qualified Data.Sequence as S (Seq(..), fromList, take, drop, dropWhileL, dropWhileR, zip, (><), index, length, (<|), mapWithIndex, tails)
import Data.Foldable (toList)

type Pot = Char

data Caven p = Caven { pots :: p, notes :: [(S.Seq Pot, Pot)] } deriving (Show, Functor)

plant = char '#'
empty = char '.'
pot = choice [plant, empty]

initialState :: GenParser Char st (S.Seq Pot)
initialState = string "initial state: " >> many1 pot >>= return . S.fromList

note :: GenParser Char st (S.Seq Pot, Pot)
note = do 
  llcrr <- count 5 pot >>= return . S.fromList
  between spaces spaces $ string "=>"
  n <- pot
  return (llcrr, n)

caven :: GenParser Char st (Caven (S.Seq Pot))
caven = do
  is <- initialState
  many newline
  ns <- note `sepEndBy` newline
  return $ Caven is (filter (('#'==) . snd) ns)

readCaven :: FilePath -> IO (Either ParseError (Caven (S.Seq Pot)))
readCaven filePath = readFile filePath >>= return . parse caven filePath

generate :: Int -> Caven (S.Seq (Int, Pot)) -> Caven (S.Seq (Int, Pot))
generate 0 caven = caven
generate n caven@(Caven indexedPots notes) = generate (n-1) $ caven { pots = trimming $ nextGen padding notes }
  where
    nextGen (_, S.Empty) _ = S.Empty
    nextGen (startIndex, indexedPots) notes = (match startIndex notes (S.take 5 indexedPots)) S.<| nextGen (startIndex + 1, S.drop 1 indexedPots) notes
    match startIndex notes pots = let pts = fmap snd pots in
                                    case {-# SCC findNote #-} find ((pts ==) . fst) notes of
                                      Just (_, pot) -> (startIndex, pot)
                                      _ -> (startIndex, '.')
    padding = let start = fst $ indexedPots `S.index` 0
                  end   = fst $ indexedPots `S.index` (S.length indexedPots - 1) in 
                (start - 2, S.fromList (zip [start - 4 .. start - 1] "....") S.>< indexedPots S.>< S.fromList (zip [end+1..end+4] "...."))
    trimming = S.dropWhileR ((/= '#') . snd) . S.dropWhileL ((/= '#') . snd) 

answerOfLv12Part f n = readCaven f
                       >>= return . right (sum . map fst . filter ((=='#') . snd) . toList . pots . generate n . fmap (S.mapWithIndex (,)))

answerOfLv12Part1 = answerOfLv12Part "input.txt" 20

answerOfLv12Part2 = answerOfLv12Part "input.txt" 5000
