module Lv5.Ans (
               ) where

import Data.List ( foldl'

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
    reactOn scannedUnits@(prevUnit:restUnits) nextUnit = if isSame prevUnit nextUnit then restUnits else nextUnit:scannedUnits

    isSame unit1 unit2 = abs (fromEnum unit1 - fromEnum unit2) == 32

answerOfLv5Part1 = loadPolymer >>= return . length . triggerReact

answerOfLv5Part2 = undefined
