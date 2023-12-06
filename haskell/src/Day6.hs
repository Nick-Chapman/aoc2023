module Day6 (main) where

import Misc (check)
import Par4 (Par,parse,separated,nl,key,int,ws0,ws1)

main :: IO ()
main = do
  s <- readFile "../input/day6-sample.input"
  i <- readFile "../input/day6.input"
  print ("day6, part1 (sam)", check 288 $ product (solve (parse gram s)))
  print ("day6, part1", check 2612736 $ product (solve (parse gram i)))
  let looseWS = filter (not . (==' '))
  print ("day6, part2 (sam)", check [71503] $ solve (parse gram (looseWS s)))
  print ("day6, part2", check [29891250] $ solve (parse gram (looseWS i)))

solve :: Setup -> [Int]
solve = map waysToWin
  where
    waysToWin (Race duration distanceToBeat) =
      length [ ()
             | n <- [0..duration]
             , let d = (duration-n) * n
             , d > distanceToBeat
             ]

type Setup = [Race]
data Race = Race Int Int deriving Show

gram :: Par Setup
gram = do
  key "Time:"; ws0; xs <- separated ws1 int; nl
  key "Distance:"; ws0; ys <- separated ws1 int; nl
  pure (zipWith Race xs ys)
