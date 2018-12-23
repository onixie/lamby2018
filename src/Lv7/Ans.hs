module Lv7.Ans () where

import Text.ParserCombinators.Parsec (parse, newline, ParseError, GenParser, string, many, between, anyChar)

loadStepsPairs :: FilePath -> IO (Either ParseError [(Char, Char)])
loadStepsPairs filePath = readFile filePath >>= return . parse (many readStepsPair) filePath

readStepsPair :: GenParser Char st (Char, Char)
readStepsPair = do
  s1 <- between (string "Step ") (string " must be finished ") anyChar
  s2 <- between (string "before step ") (string " can begin.") anyChar
  newline
  return (s1, s2)
  
answerOfLv7Part1 = undefined
answerOfLv7Part2 = undefined
