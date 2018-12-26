module Lv5.Ans (answerOfLv5Part1, answerOfLv5Part2) where

import Data.List (minimumBy, maximumBy, delete, nub, sort, sortOn, deleteBy, nubBy,  foldl'

                 )

import Data.Ix ( inRange
               )

type Unit = Char
type Polymer = [Unit]

loadPolymer :: IO Polymer
loadPolymer = readFile "input.txt"

triggerReact :: Polymer -> Polymer
triggerReact = reverse . foldl' reactOn []
  where
    reactOn scannedUnits '\n' = scannedUnits
    reactOn scannedUnits@[] nextUnit = [nextUnit]
    reactOn scannedUnits@(prevUnit:restUnits) nextUnit = if prevUnit `canReactOn` nextUnit then restUnits else nextUnit:scannedUnits
    canReactOn unit1 unit2 = abs (fromEnum unit1 - fromEnum unit2) == 32

removePolarity :: Unit -> Unit
removePolarity unit = if inRange ('a', 'z') unit then toEnum $ fromEnum unit - 32 else unit

hasSameUnitType :: Unit -> Unit -> Bool
hasSameUnitType unit1 unit2 = unit1 == unit2 || abs (fromEnum unit1 - fromEnum unit2) == 32

answerOfLv5Part1 = loadPolymer >>= return . length . triggerReact

listTypes :: Polymer -> [Unit]
listTypes = delete '\n'. nub . sort . map removePolarity

removeUnits :: Unit -> Polymer -> Polymer
removeUnits unit = filter (not . hasSameUnitType unit)

findShortestMutatedPolymer polymer = let types = listTypes polymer
                                         mutations = map (flip removeUnits polymer) types in
                                       minimumBy (\p1 p2 -> length(p1) `compare` length(p2)) (map triggerReact mutations)

answerOfLv5Part2 = loadPolymer >>= return . length . findShortestMutatedPolymer
