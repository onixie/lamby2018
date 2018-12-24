module Lv7.Ans () where

import Text.ParserCombinators.Parsec (parse, newline, ParseError, GenParser, string, many, between, anyChar)
import Data.List (nub, sort, elem)
import Control.Exception (assert)
import Control.Arrow (right)

newtype Step = Step { name :: Char} deriving (Show, Eq, Ord)
newtype Instruction = Instruction { readInstruction :: (Step, Step) } deriving (Show, Eq)

loadInstructions :: FilePath -> IO (Either ParseError [Instruction])
loadInstructions filePath = readFile filePath >>= return . parse (many parseInstruction) filePath

parseInstruction :: GenParser Char st Instruction
parseInstruction = do
  s1 <- between (string "Step ") (string " must be finished ") anyChar
  s2 <- between (string "before step ") (string " can begin.") anyChar
  newline
  return $ Instruction (Step s1, Step s2)

allSteps instructions = nub . sort $ map (fst . readInstruction) instructions ++ map (snd . readInstruction) instructions

initialSteps instructions = sort . filter (not . flip elem (map (snd . readInstruction) instructions)) $ allSteps instructions

previousSteps instructions step = sort $ map fst $ filter ((step==) . snd) (map readInstruction instructions)

nextSteps instructions step = sort $ map snd $ filter ((step==) . fst) (map readInstruction instructions)

nextAvailableSteps :: [Instruction] -> [Step] -> Step -> [Step]
nextAvailableSteps instructions availableSteps step = assert (elem step availableSteps) . nub . sort $ 
  filter (all ((flip elem) availableSteps) . previousSteps instructions) (nextSteps instructions step)

go :: [Instruction] -> [Step]
go = walk ([]) =<< initialSteps
  where
    walk forward availables@(a:as) instructions = walk (a:forward) (nub . sort$as++(nextAvailableSteps instructions (a:forward) a)) (remove a fst instructions)
    walk forward availables [] = reverse forward
    remove step f instructions = filter ((step/=) . f . readInstruction) instructions

answerOfLv7Part1 = loadInstructions "input.txt" >>= return . right (map name . go)
answerOfLv7Part2 = undefined
