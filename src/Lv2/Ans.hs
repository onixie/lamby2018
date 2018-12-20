module Lv2.Ans
  ( answerOfLv2Part1, answerOfLv2Part2
  ) where
import Data.List (group, sort, find, intersect)

-- lv2
type Id = String

loadBoxIds :: IO [Id]
loadBoxIds = readFile "input.txt" >>= return . lines

-- part1

countLetters :: Id -> [Int]
countLetters id = fmap length . group $ sort id

occursIn :: Int -> [[Int]] -> Int
occursIn n ns = length $ filter (elem n) ns

calcCheckSum :: [Id] -> Int
calcCheckSum ids = 
  let counts = countLetters <$> ids in
    2 `occursIn` counts * 3 `occursIn` counts

answerOfLv2Part1 = loadBoxIds >>= return . calcCheckSum

--part2

hasOneDiff :: Id -> Id -> Bool
hasOneDiff = ode 0
  where
    ode 1 [] [] = True
    ode _ [] [] = False
    ode c (l:ls) (r:rs)
      | l /= r && c == 1 = False
      | l /= r = ode 1 ls rs
      | otherwise = ode c ls rs

findExactlyOneDiff :: [Id] -> (Id, Id)
findExactlyOneDiff (id:restIds) = case find (id `hasOneDiff`) restIds of
  Just found -> (id, found)
  Nothing -> findExactlyOneDiff restIds

answerOfLv2Part2 = loadBoxIds >>= return . findExactlyOneDiff
