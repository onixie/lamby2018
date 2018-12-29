module Lv11.Ans (answerOfLv11Part1, answerOfLv11Part2) where

import Data.Array (Array, array, (!), assocs)
import Data.List (maximumBy)

type PowerLevel = Int

gridOfFuelCells :: Int -> Array (Int, Int) PowerLevel
gridOfFuelCells serialNumber = array ((1, 1), (300, 300)) [ ((x, y), powerLevel x y serialNumber) | x <- [1..300], y <- [1..300] ]
  where
    powerLevel x y sn = let rackId = x + 10
                            startOfLv = (rackId * y + sn) * rackId
                            digitInHundreds = startOfLv `div` 100 - startOfLv `div` 1000 * 10 in
      digitInHundreds - 5

gridOfTotalPowerLevel :: Array (Int, Int) PowerLevel -> Array (Int, Int) PowerLevel
gridOfTotalPowerLevel grid = array ((1, 1), (298, 298)) [ ((x,y)
                                                  ,   grid ! (x, y)   + grid ! (x+1, y)   + grid ! (x+2, y)
                                                    + grid ! (x, y+1) + grid ! (x+1, y+1) + grid ! (x+2, y+1)
                                                    + grid ! (x, y+2) + grid ! (x+1, y+2) + grid ! (x+2, y+2)
                                                  ) | x <- [1..298], y <- [1..298]]

largestTotalPower = maximumBy ((. snd) . compare . snd) . assocs . gridOfTotalPowerLevel . gridOfFuelCells

answerOfLv11Part1 = largestTotalPower 5719


answerOfLv11Part2 = undefined
