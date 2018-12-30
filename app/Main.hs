module Main where

--import Lv9.Ans
--import Lv10.Ans
import Lv12.Ans

main :: IO ()
--main = answerOfLv9Part "/Users/shen/workdir/lamby2018/src/Lv9/sample.txt" 3 >>= print
--main = answerOfLv10Part1 "/Users/shen/workdir/lamby2018/src/Lv10/input.txt"
main = answerOfLv12Part "/Users/shen/workdir/lamby2018/src/Lv12/input.txt" 50000 >>= print >> return ()
