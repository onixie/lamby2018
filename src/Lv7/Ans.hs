module Lv7.Ans (answerOfLv7Part1, answerOfLv7Part2) where

import Text.ParserCombinators.Parsec (parse, newline, ParseError, GenParser, string, many, between, anyChar)
import Data.List (partition, nub, sort, elem)
import Control.Exception (assert)
import Control.Arrow (right)
import Data.Char (ord)
import Data.Ix (inRange)

newtype Step = Step Char deriving (Show, Eq, Ord)
newtype Instruction = Instruction { readInstruction :: (Step, Step) } deriving (Show, Eq)

loadInstructions :: FilePath -> IO (Either ParseError [Instruction])
loadInstructions filePath = readFile filePath >>= return . parse (many parseInstruction) filePath

parseInstruction :: GenParser Char st Instruction
parseInstruction = do
  s1 <- between (string "Step ") (string " must be finished ") anyChar
  s2 <- between (string "before step ") (string " can begin.") anyChar
  newline
  assert (all (inRange ('A', 'Z')) [s1, s2]) . return $ Instruction (Step s1, Step s2)

allSteps instructions = nub . sort $ map (fst . readInstruction) instructions ++ map (snd . readInstruction) instructions

initialSteps instructions = sort . filter (not . flip elem (map (snd . readInstruction) instructions)) $ allSteps instructions

previousSteps instructions step = sort $ map fst $ filter ((step==) . snd) (map readInstruction instructions)

nextSteps instructions step = sort $ map snd $ filter ((step==) . fst) (map readInstruction instructions)

nextAvailableSteps :: [Instruction] -> [Step] -> Step -> [Step]
nextAvailableSteps instructions availableSteps step = assert (elem step availableSteps) . nub . sort $ 
  filter (all ((flip elem) availableSteps) . previousSteps instructions) (nextSteps instructions step)

build :: [Instruction] -> [Step]
build = make [] =<< initialSteps
  where
    make done availables@(a:as) instructions = make (a:done) (nub . sort $ as++(nextAvailableSteps instructions (a:done) a)) (remove [a] fst instructions)
    make done availables [] = reverse done

remove steps f instructions = filter ((not . (flip elem) steps) . f . readInstruction) instructions

nameOf (Step s) = s

answerOfLv7Part1 = loadInstructions "input.txt" >>= return . right (map nameOf . build)

timeCost :: Step -> Int
timeCost (Step a)= ord a - ord 'A' + 1 + 60

data Worker = Worker { id :: Int, works :: [Step] } deriving (Show)

isIdle (Worker _ works) | works == [] = True
                        | otherwise = False

findIdle workers = filter isIdle workers

assign workers works = let (idleWorkers, workingWorkers) = partition isIdle workers
                           assignedWorkers = zipWith (\wkr wk -> wkr{works=replicate (timeCost wk) wk}) idleWorkers works
                           remainedWorks = drop (length idleWorkers `min` length works) works
                           remainedIdleWorkers = drop (length assignedWorkers) idleWorkers in
                         (workingWorkers++assignedWorkers++remainedIdleWorkers, remainedWorks)

progress1 second workers = let toBeDone = nub . sort . concatMap works $ filter ((==1). length . works) workers
                               newWorkers = (map (\wkr -> if isIdle wkr then wkr else wkr{works=drop 1 (works wkr)}) workers) in 
                             if toBeDone /= []
                             then ((second+1, newWorkers), toBeDone)
                             else progress1 (second+1) newWorkers 

buildTogether :: [Worker] -> [Instruction] -> IO (Int, [Step])
buildTogether workers = make 0 [] workers =<< initialSteps 
  where
    make second done _ [] [] = return (second, done)
    make second done workers availables instructions = do
      let (wkrs, rwks) = assign workers availables
      let ((sec, wkrs'), finish) = progress1 second wkrs
      let done' = (done++finish)
      let nextAvails = (nub . sort $ rwks ++ (concatMap (nextAvailableSteps instructions done') finish))
      print ((sec, wkrs), finish) >> make sec done' wkrs' nextAvails (remove done' fst instructions)

answerOfLv7Part2 = do Right is <- loadInstructions "input.txt"
                      buildTogether [Worker id [] | id <- [1..5]] is
