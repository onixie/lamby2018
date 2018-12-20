module Lv3.Ans
  ( answerOfLv3Part1
  , answerOfLv3Part2
  ) where

import Text.ParserCombinators.Parsec ( GenParser
                                     , ParseError
                                     , many
                                     , newline
                                     , digit
                                     , spaces
                                     , char
                                     , parse
                                     )
import Data.Array ( Array
                  , array
                  , elems 
                  , (//)
                  , (!)
                  )

import Control.Arrow ( right
                     )

type Id = Int
data Claim = Claim { id :: Id
                   , x :: Int
                   , y :: Int
                   , width :: Int
                   , height :: Int
                   } deriving (Show)

loadClaims :: IO (Either ParseError [Claim])
loadClaims = readFile "input.txt" >>= return . parseClaims

parseClaims :: String -> Either ParseError [Claim]
parseClaims text = parse claims "" text
  where
    claims :: GenParser Char st [Claim]
    claims = many (claim <* newline)
    claim :: GenParser Char st Claim
    claim = do 
      char '#'
      id <- read <$> many digit
      spaces >> char '@' >> spaces
      x <- read <$> many digit
      char ','
      y <- read <$> many digit
      spaces >> char ':' >> spaces
      width <- read <$> many digit
      char 'x'
      height <- read <$> many digit
      return $ Claim id x y width height

-- overlap :: Claim -> Claim -> Maybe (Int, Int, Int, Int)
-- overlap (Claim id left top width height) (Claim id' left' top' width' height') =
--   do (ovLeft, ovRight) <- overlapping left (left + width) left' (left' + width')
--      (ovTop, ovBottom) <- overlapping top (top + height) top' (top' + height')
--      return (ovLeft, ovTop, ovRight - ovLeft, ovBottom - ovTop)
--   where 
--     overlapping s1 e1 s2 e2 = -- assume s1 > e1, s2 > e2
--       let s' = s1 `max` s2
--           e' = e1 `min` e2 in
--         if s' < e' then Just (s', e') else Nothing

type Bound = ((Int, Int), (Int, Int))
bound :: [Claim] -> Bound
bound = foldr (\(Claim _ x y width height) ((lx, ly), (hx, hy)) -> 
                  ((min lx x, min ly y), (max hx (x + width), max hy (y + height)))) ((0, 0), (0, 0))
  
type Fabric = Array (Int, Int) Int

fabric :: Bound -> Fabric
fabric ((lx, ly), (hx, hy)) = array ((lx, ly), (hx, hy)) [((x, y), 0) | x <- [lx..hx], y <- [ly..hy]]

patch :: Fabric -> Claim -> Fabric
patch fabric (Claim id x y width height) = 
  fabric // [ ((x', y'), fabric ! (x', y') + 1) | x' <- [x..(x + width - 1)], y' <- [y..(y + height - 1)] ]

patchAll :: Fabric -> [Claim] -> Fabric
patchAll = foldr (flip patch)

squareInchesOfOverlapping :: Fabric -> [Claim] -> Int
squareInchesOfOverlapping = ((length . filter (>1) . elems) .) . patchAll

answerOfLv3Part1 = loadClaims >>= return . right (squareInchesOfOverlapping =<< (fabric . bound))

answerOfLv3Part2 = undefined
