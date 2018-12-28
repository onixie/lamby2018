{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Lv10.Ans (answerOfLv10Part1, answerOfLv10Part2) where

import Text.ParserCombinators.Parsec
import Data.Char
import Control.Applicative (liftA2)
import Control.Arrow (right)

import Diagrams.Prelude (Diagram, circle, rect, fc, (#), lc, place, moveOriginTo)

--import Diagrams.Backend.SVG.CmdLinea
--import Diagrams.Backend.SVG
import Diagrams.TwoD.Size
import Diagrams.TwoD
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Backend.Rasterific
import Diagrams.Combinators (position, atop, atPoints)

import Data.Colour.Names (black, green, white, red)

loadPoints filePath = readFile filePath >>= return . parse (point `sepEndBy` newline) ""

type Position = (Double, Double)
type Velocity = (Double, Double)
type Point = (Position, Velocity)

point :: GenParser Char st Point
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

viewBounds points = let xs = map (fst . fst) points
                        ys = map (snd . fst) points
                        left = minimum xs
                        top  = minimum ys
                        right = maximum xs
                        bottom = maximum xs in
  ((left, top), (right, bottom))

viewSize points = let ((left, top), (right, bottom)) = viewBounds points in
  (right - left, bottom - top)

viewCenter points = let ((left, top), (right, bottom)) = viewBounds points in
  ((right + left) / 2.0, (bottom + top) / 2.0)

viewRadius points = let ((left, top), (right, bottom)) = viewBounds points
                        (cx, cy) = viewCenter points in
  ceiling . sqrt $ ((cx - left)^2 + (cy - top)^2)

move :: [Point] -> Int -> [Point]
move points n
  | n > 0 = move (map step points) (n-1)
  | otherwise = points
  where
    step ((x, y), (vx, vy)) = ((x+vx,y+vy),(vx, vy))

view :: [((Double, Double),(Double, Double))] -> Diagram B
view points = position [ (pos, circle 1 # lc red # fc black) | pos <- map (p2 . fst) points ] `atop` position [(p2 (viewCenter points), (uncurry rect) (viewSize points) # lc black # showOrigin )]

generateAnimatedView points timeRange = gifMain [(view(move points i) # fc white, i) | i <- timeRange]

answerOfLv10Part1 filePath = do Right pts <- loadPoints filePath >>= return . right ((flip move) 10880)
                                generateAnimatedView pts [0..30]

findMinimumHeight points = fmh points 0
  where 
    fmh pts n = let pts0 = move pts 0
                    pts1 = move pts0 1 in
                  if snd (viewSize pts0) > snd (viewSize pts1)
                  then fmh pts1 (n+1)
                  else n

drawView points = renderRasterific "./answerLv10Part2.png" (dims2D 500 500) (view points)

answerOfLv10Part2 = do Right pts <- loadPoints "input.txt"
                       let sec = findMinimumHeight pts -- by the assumption that when the words appear, the height of the bounds is the smallest of all the other time
                       print sec
                       drawView $ move pts sec
