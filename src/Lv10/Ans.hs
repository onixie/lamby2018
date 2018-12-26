module Lv10.Ans (answerOfDay10Part1, answerOfDay10Part2) where

import Text.ParserCombinators.Parsec
import Data.Char
import Control.Applicative (liftA2)

loadPoints filePath = readFile filePath >>= return . parse (point `sepEndBy` newline) ""

point :: GenParser Char st ((Int, Int),(Int,Int))
point = do
  pos <- between (string "position=<") (string ">") pairOfNumbers
  many space
  vel <- between (string "velocity=<") (string ">") pairOfNumbers
  return (pos, vel)
  where 
    pairOfNumbers = do x <- many space *> number
                       char ','
                       y <- many space *> number
                       return (x, y)
    number = liftA2 (++) (option "" (string "-")) (many1 (satisfy isDigit)) >>= return . read

answerOfDay10Part1 = undefined
answerOfDay10Part2 = undefined
