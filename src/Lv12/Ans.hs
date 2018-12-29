module Lv12.Ans () where

import Text.ParserCombinators.Parsec (ParseError, parse, sepEndBy, between, count, GenParser, string, many, many1, choice, char)
import Text.Parsec.Char (spaces, newline)

type Pot = Char

data Caven = Caven { initial :: [Pot], notes :: [([Pot], Pot)] } deriving (Show)

plant = char '#'
empty = char '.'
pot = choice [plant, empty]

initialState :: GenParser Char st [Pot]
initialState = string "initial state: " >> many1 pot

note :: GenParser Char st ([Pot], Pot)
note = do 
  llcrr <- count 5 pot
  between spaces spaces $ string "=>"
  n <- pot
  return (llcrr, n)

caven :: GenParser Char st Caven
caven = do
  is <- initialState
  many newline
  ns <- note `sepEndBy` newline
  return $ Caven is ns

readPots :: FilePath -> IO (Either ParseError Caven)
readPots filePath = readFile filePath >>= return . parse caven filePath
