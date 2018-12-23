module Lv6.Ans (
               ) where

import Text.ParserCombinators.ReadP (eof, ReadP, many, many1, skipSpaces, char, satisfy, readP_to_S)
import Data.Char (isDigit)

readCoordinate :: ReadP (Integer, Integer)
readCoordinate = do x <- number >>= return . read
                    skipSpaces *> char ','*> skipSpaces
                    y <- number >>= return . read
                    char '\n'
                    return (x, y)
  where
    number = many1 $ satisfy isDigit

readCoordinates = many readCoordinate <* eof

loadCoordinates :: IO [(Integer, Integer)]
loadCoordinates = readFile "input.txt" >>= return . fst . (!!0) . readP_to_S readCoordinates

answerOfLv6Part1 = undefined

answerOfLv6Part2 = undefined
