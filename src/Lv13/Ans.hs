module Lv13.Ans () where

import Control.Arrow (second, first)
import Data.Array (Array, array, assocs, bounds, elems, (!), (//))
import Data.List (sortOn, filter, map)

data Cart = Cart (Int, Int) Char deriving (Show)
data System = System { tracks :: Array (Int, Int) Char, carts :: [Cart] }

readSystem = readFile "input.txt" >>= makeSystem . lines

makeSystem inputs = do 
  let tracks = array ((-1, -1), (width+1, height+1)) [((x, y), ' ')| x<-[-1..width+1], y<-[-1..height+1]] -- add empty columns and rows around the tracks to make later life easier
               // [((x, y), c)| x<-[0..width], y<-[0..height], let c = inputs!!y!!x]
  return . restorePathUnderCarts . System tracks . map (uncurry Cart) . filter (isCart . snd) $ assocs tracks
  where
    width = length (inputs!!0) - 1
    height = length inputs - 1

restorePathUnderCarts system@(System tracks carts) = foldl restore system carts
  where 
    restore system@(System tracks _) cart = system{tracks = update tracks cart}
    update tracks cart@(Cart (x, y) _) = tracks//[((x, y), tracks `pathUnder` cart)]

instance Show System where
  show (System tracks carts) = let ((_, _), (_, height)) = bounds tracks in
                                 "Tracks\n" ++
                                 "------\n" ++
                                 concatMap (++"\n") [ xs | y <- [-1..height+1], let xs = map snd . sortOn (fst . fst) . filter ((y==) . snd . fst) $ assocs tracks ] ++
                                 "Carts\n" ++
                                 "-----\n" ++
                                 concatMap (\cart@(Cart loc _) -> show cart ++ "\n" ++ (tracks `surroundAt` cart)) carts 

isCart c = any (c==) "<>^v"

surroundAt tracks cart@(Cart (x,y) c) = let n = tracks!(x, y-1)
                                            s = tracks!(x, y+1)
                                            e = tracks!(x+1, y)
                                            w = tracks!(x-1, y)
                                            p = tracks `pathUnder` cart 
  in
  [' ', n, ' ', '|', ' ', n, ' ', '\n'] ++
  [ w , c,  e , '|',  w , p,  e , '\n'] ++
  [' ', s, ' ', '|', ' ', s, ' ', '\n']

-- Assume: no carts are at intersections and curves, so this is a partial function
pathUnder tracks (Cart loc@(x, y) c) = if tracks!(x, y) `elem` "-|+/\\"
                                           then tracks!(x, y)
                                           else let e = move loc towardEast  (until "-+<>")
                                                    w = move loc towardWest  (until "-+<>")
                                                    n = move loc towardNorth (until "|+v^")
                                                    s = move loc towardSouth (until "|+v^") in
                                               
                                                  if isPath e w && not (isPath n s)
                                                  then '-'
                                                  else if isPath n s && not (isPath e w)
                                                  then '|'
                                                  else if isCurveBackSlash s w || isCurveBackSlash n e
                                                  then '\\'
                                                  else if isCurveSlash w n || isCurveSlash e s
                                                  then '/'
                                                  else '?'
    where
      move loc dir stopAt = let c = tracks!loc in
        if stopAt c
        then c
        else move (dir loc) dir stopAt
      towardNorth = second (subtract 1)
      towardSouth = second (+1)
      towardWest  = first  (subtract 1)
      towardEast  = first  (+1)
      until = (flip notElem)
      isPath f b = (f =='/' && b == '\\') || (f == '\\' && b =='/')
      isCurveBackSlash f b = f =='/' && b == '/'
      isCurveSlash f b = f == '\\' && b =='\\'
