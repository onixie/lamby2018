module Lv8.Ans () where

import Text.ParserCombinators.Parsec (ParseError, parse, many1, many, digit, sepEndBy, space )
import Control.Arrow (right)
import Data.Ix (inRange)
import Data.List (splitAt)
import qualified Data.Tree as T (Tree(..), flatten, foldTree, drawTree)
import Control.Exception (assert)

loadLicenseFile :: FilePath -> IO (Either ParseError [Int])
loadLicenseFile filePath = readFile filePath >>= return . parse (number `sepEndBy` space) filePath
  where
    number = many1 digit >>= return . read

data Node = Node { header :: (Int, Int), metaEntries :: [Int] } deriving (Show, Eq)
type Tree = T.Tree Node

buildTree :: [Int] -> IO (Tree, [Int])
buildTree (numOfChildren:numOfMetaEntries:rest) = do rs <- buildChildTrees numOfChildren rest
                                                     let rest' = if rs == [] then rest else snd $ last rs
                                                     let children = map fst rs
                                                     let (metaEntries, rest'') = splitAt numOfMetaEntries rest'
                                                     return ( T.Node { T.rootLabel = Node { header =(numOfChildren, numOfMetaEntries)
                                                                                        , metaEntries = metaEntries
                                                                                        }
                                                                     , T.subForest = children
                                                                     }
                                                            , rest'')
  where
    buildChildTrees 0 rest = return []
    buildChildTrees numOfChildren rest = do (child, rest') <- buildTree rest
                                            res <- buildChildTrees (numOfChildren - 1) rest'
                                            return $ (child, rest'):res

answerOfLv8Part1 = do Right serialNumbers <- loadLicenseFile "input.txt"
                      (root, rest) <- buildTree serialNumbers
                      let allMetaEntries = assert (rest == []) $ concatMap metaEntries (T.flatten root)
                      return $ foldl1 (+) allMetaEntries

calcValue (Node (numOfChildren, numOfMetaEntries) metaEntries) subValues = foldl (+) 0 values
  where 
    values = if numOfChildren == 0
             then metaEntries
             else map (subValues!!) (filter (inRange (0, numOfChildren - 1)) (map (negate 1+) metaEntries))

answerOfLv8Part2 = do Right serialNumbers <- loadLicenseFile "input.txt"
                      (root, rest) <- buildTree serialNumbers
                      -- putStr $ T.drawTree $ fmap show root 
                      return . assert (rest == []) $ T.foldTree calcValue root
                      
