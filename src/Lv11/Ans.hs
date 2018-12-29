module Lv11.Ans (answerOfLv11Part1, answerOfLv11Part2) where

import Data.Array (Array, array, (!), assocs)
import Data.List (maximumBy)
import Data.Tuple (swap)
import Control.Concurrent (withMVar, newMVar, takeMVar, putMVar, forkFinally, newEmptyMVar, forkIO)

type PowerLevel = Int

gridOfFuelCells :: Int -> Array (Int, Int) PowerLevel
gridOfFuelCells serialNumber = array ((1, 1), (300, 300)) [ ((x, y), powerLevel x y serialNumber) | x <- [1..300], y <- [1..300] ]
  where
    powerLevel x y sn = let rackId = x + 10
                            startOfLv = (rackId * y + sn) * rackId
                            digitInHundreds = startOfLv `div` 100 - startOfLv `div` 1000 * 10 in
      digitInHundreds - 5

gridOfTotalPower :: Int -> Array (Int, Int) PowerLevel -> [((Int, Int), PowerLevel)]
gridOfTotalPower n grid = let gridSize = 301 - n
                              gridIndices = [ 1 .. gridSize ] 
                              squareIndices = [ 0 .. n - 1 ] in
                            [ ((x, y), sum [ grid ! (x + dx, y + dy) 
                                            | dx <- squareIndices, dy <- squareIndices ]) 
                            | x <- gridIndices, y <- gridIndices ]

findLargestPower = maximumBy ((. snd) . compare . snd) 

totalPower grid x y dx dy
  | dy > 1 = calcRowPower grid x y dx + totalPower grid x (y+1) dx (dy-1)
  | otherwise = calcRowPower grid x y dx
  where 
    calcRowPower grid x y dx
      | dx > 1 = grid ! (x, y) + calcRowPower grid (x+1) y (dx-1)
      | otherwise = grid ! (x, y)

gridOfTotalPower' n grid = let range = [1..301 - n] in 
  [((x,y), totalPower grid x y n n) | x <- range, y <- range]

largestTotalPower n = findLargestPower . gridOfTotalPower n

answerOfLv11Part1 = largestTotalPower 3 $ gridOfFuelCells 5719

onTotalPowers grid n m print
  | n <= m = do
      forkFinally (return (largestTotalPower n grid)) (\ltp -> print (n, ltp)) >> onTotalPowers grid (n+1) m print
  | otherwise = return ()

answerOfLv11Part2 :: IO ()
answerOfLv11Part2 = do lock <- newMVar ()
                       let atomicPrint str = withMVar lock (\_ -> print str)
                       onTotalPowers (gridOfFuelCells 5719) 1 300 atomicPrint
                       return ()
