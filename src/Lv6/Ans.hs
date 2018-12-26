module Lv6.Ans (answerOfLv6Part1, answerOfLv6Part2) where

import Text.ParserCombinators.ReadP (count, eof, ReadP, many, many1, skipSpaces, char, satisfy, readP_to_S)
import Data.Char (isDigit)
import Data.Array (bounds, assocs, elems, array, Array)

import Data.List (maximumBy, groupBy, sortOn, elemIndices, minimumBy)

type Coord = (Integer, Integer)
readCoordinate :: ReadP Coord
readCoordinate = do x <- number >>= return . read
                    skipSpaces *> char ','*> skipSpaces
                    y <- number >>= return . read
                    char '\n'
                    return (x, y)
  where
    number = many1 $ satisfy isDigit

readCoordinates = many readCoordinate <* eof

loadCoordinates :: IO [Coord]
loadCoordinates = readFile "input.txt" >>= return . fst . (!!0) . readP_to_S readCoordinates

bound :: [Coord] -> (Coord, Coord)
bound coords = let xs = map fst coords
                   ys = map snd coords in
                 ((minimum xs, minimum ys), (maximum xs, maximum ys))

manhattanDistance :: Coord -> Coord -> Integer
manhattanDistance (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

generateMap :: [Coord] -> Array Coord Int
generateMap coords = let bnd@((left,top), (right, bottom)) = (bound coords) in 
  array bnd [((x, y), ci) | i <- [0..length coords]
                          , x <- [left .. right]
                          , y <- [top ..  bottom]
                          , let ci = findClosest coords (x, y) ]

findClosest :: [Coord] -> Coord -> Int
findClosest coords coord = let distances = map (manhattanDistance coord) coords
                               minDist   = minimum distances
                               closest   = elemIndices minDist distances in
                             if length closest == 1 
                             then closest!!0
                             else -1

--findLargestFiniteArea :: [Coord] -> Coord
findLargestFiniteArea coords = let m = assocs $ generateMap coords
                                   ((left,top), (right,bottom)) = bound coords
                                   m0 = filter ((/=(-1)) . snd) m
                                   m1 = filter ((/=left) . fst . fst) m0
                                   m2 = filter ((/=right) . fst . fst) m1
                                   m3 = filter ((/=top) . snd . fst) m2
                                   m4 = filter ((/=bottom) . snd . fst) m3
                                   groupedArea = groupBy (\x y -> snd x == snd y) $ sortOn snd m4 in
                                 -- reverse $ sortOn length groupedArea 
                                 maximumBy (\g1 g2 -> length g1 `compare` length g2) groupedArea

answerOfLv6Part1 = loadCoordinates >>= return . length . findLargestFiniteArea


generateMap2 :: [Coord] -> Array Coord Bool
generateMap2 coords = let bnd@((left,top), (right, bottom)) = (bound coords) in 
  array bnd [((x, y), safe) | i <- [0..length coords]
                          , x <- [left .. right]
                          , y <- [top ..  bottom]
                          , let safe = if locationWithDistance (<10000) coords (x, y) then True else False ]

locationWithDistance f coords coord = let distances = map (manhattanDistance coord) coords in
                                        f $ sum distances 
                                            
findSafeRegionArea coords = let m = assocs $ generateMap2 coords
                                m0 = filter ((==True) . snd) m in
                              m0

answerOfLv6Part2 = loadCoordinates >>= return . length . findSafeRegionArea

