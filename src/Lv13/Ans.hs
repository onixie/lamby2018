{-# LANGUAGE NamedFieldPuns #-}
module Lv13.Ans () where

import Control.Arrow (second, first)
import Data.Array (Array, array, assocs, bounds, elems, (!), (//))
import Data.List (group, nub, sortOn, filter, map)
import Data.Function (on, (&))
import Data.Tuple (swap)

data Dir = L | S | R deriving (Show, Eq, Ord, Enum)
data Cart = Cart { dir :: Dir, loc :: (Int, Int), face :: Char } deriving (Show)
data System = System { tick :: Int, tracks :: Array (Int, Int) Char, carts :: [Cart] }

readSystem = readFile "input.txt" >>= makeSystem . lines

makeSystem inputs = do 
  let tracks = array ((-1, -1), (width+1, height+1)) [((x, y), ' ')| x<-[-1..width+1], y<-[-1..height+1]] -- add empty columns and rows around the tracks to make later life easier
               // [((x, y), c)| x<-[0..width], y<-[0..height], let c = inputs!!y!!x]
  return . restorePathUnderCarts . System 0 tracks . sort . map (uncurry (Cart L)) . filter (isCart . snd) $ assocs tracks
  where
    width = length (inputs!!0) - 1
    height = length inputs - 1

restorePathUnderCarts system@System{tracks, carts} = foldl restore system carts
  where 
    restore system@System{tracks} cart = system{tracks = update tracks cart}
    update tracks cart@Cart{loc=(x, y)} = tracks//[((x, y), tracks `pathUnder` cart)]

instance Show System where
  show System{tick, tracks, carts} = let ((_, _), (_, height)) = bounds tracks in
                                 "Tick\n" ++
                                 "----\n" ++
                                 show tick ++ "\n\n" ++
                                 "Tracks\n" ++
                                 "------\n" ++
                                 concatMap (++"\n") [ xs | y <- [-1..height+1], let xs = map snd . sortOn (fst . fst) . filter ((y==) . snd . fst) $ assocs tracks ] ++
                                 "Carts\n" ++
                                 "-----\n" ++
                                 concatMap (\cart -> show cart ++ "\n" ++ (tracks `surroundAt` cart)) carts 

isCart c = any (c==) "<>^v"

surroundAt tracks cart@Cart{loc=(x, y), face=c} = let n = tracks!(x, y-1)
                                                      s = tracks!(x, y+1)
                                                      e = tracks!(x+1, y)
                                                      w = tracks!(x-1, y)
                                                      p = tracks `pathUnder` cart in
  [' ', n, ' ', '|', ' ', n, ' ', '\n'] ++
  [ w , c,  e , '|',  w , p,  e , '\n'] ++
  [' ', s, ' ', '|', ' ', s, ' ', '\n']

-- Assume: no carts are at intersections and curves, so this is a partial function
pathUnder tracks Cart{loc=loc@(x, y)} = if tracks!(x, y) `elem` "-|+/\\"
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

move tracks cart@Cart{loc=(x, y), face} = case face of
  '>' -> case tracks!(x+1, y) of
           '-'  -> cart{loc=(x+1, y)}
           '/'  -> cart{loc=(x+1, y),face='^'}
           '\\' -> cart{loc=(x+1, y),face='v'}
           '+'  -> cart{loc=(x+1, y)} & turn
  '<' -> case tracks!(x-1, y) of
           '-'  -> cart{loc=(x-1, y)}
           '/'  -> cart{loc=(x-1, y),face='v'}
           '\\' -> cart{loc=(x-1, y),face='^'}
           '+'  -> cart{loc=(x-1, y)} & turn
  '^' -> case tracks!(x, y-1) of
           '|'  -> cart{loc=(x, y-1)}
           '/'  -> cart{loc=(x, y-1),face='>'}
           '\\' -> cart{loc=(x, y-1),face='<'}
           '+'  -> cart{loc=(x, y-1)} & turn
  'v' -> case tracks!(x, y+1) of
           '|'  -> cart{loc=(x, y+1)}
           '/'  -> cart{loc=(x, y+1),face='<'}
           '\\' -> cart{loc=(x, y+1),face='>'}
           '+'  -> cart{loc=(x, y+1)} & turn
  where
   turn cart@Cart{face='>', dir=L} = cart{face='^', dir=S}
   turn cart@Cart{face='^', dir=L} = cart{face='<', dir=S}
   turn cart@Cart{face='<', dir=L} = cart{face='v', dir=S}
   turn cart@Cart{face='v', dir=L} = cart{face='>', dir=S}
   turn cart@Cart{face='>', dir=S} = cart{face='>', dir=R}
   turn cart@Cart{face='^', dir=S} = cart{face='^', dir=R}
   turn cart@Cart{face='<', dir=S} = cart{face='<', dir=R}
   turn cart@Cart{face='v', dir=S} = cart{face='v', dir=R}
   turn cart@Cart{face='>', dir=R} = cart{face='v', dir=L}
   turn cart@Cart{face='v', dir=R} = cart{face='<', dir=L}
   turn cart@Cart{face='<', dir=R} = cart{face='^', dir=L}
   turn cart@Cart{face='^', dir=R} = cart{face='>', dir=L}

tick1 system@System{tracks, carts, tick} = system{carts=sort $ map (move tracks) carts, tick=tick+1}

loop n system@System{carts} | hasCollision carts = system
                            | n > 0 = loop (n-1) $ system & tick1
                            | otherwise = system
  where hasCollision carts = (/=) <$> nub <*> id $ (map loc carts)

sort = sortOn (swap . loc)

answerOfLv13Part1 = do 
  System{carts} <- readSystem >>= return . loop 1000
  return . (!!0) . filter ((>1).length) . group $ map loc carts
  
