module Day6 (main) where

import Misc (check)
import Par4 (Par,parse,separated,nl,key,int,ws0,ws1)

main :: IO ()
main = do
  s <- readFile "../input/day6-sample.input"
  i <- readFile "../input/day6.input"
  print ("day6, part1 (sam)", check 288 $ product (map solve (parse gram s)))
  print ("day6, part1", check 2612736 $ product (map solve (parse gram i)))
  let looseWS = filter (not . (==' '))
  print ("day6, part2 (sam)", check [71503] $ map solve (parse gram (looseWS s)))
  print ("day6, part2", check [29891250] $ map solve (parse gram (looseWS i)))

solve :: Race -> Int
solve (Race duration distanceToBeat) = _fast -- _slow
  where
    f n = (duration-n) * n > distanceToBeat
    _slow = length [ () | n <- [0..duration] , f n ]
    _fast = let x = search f (0,duration `div` 2) in (duration - x) - x + 1

search :: (Int -> Bool) -> (Int,Int) -> Int
search f (a,b) = -- search for positive edge; invariant: !f(a) && f(b)
  if a+1 == b then b else do
    let m = (a+b) `div` 2
    search f $ if f m then (a,m) else (m,b)

type Setup = [Race]
data Race = Race Int Int deriving Show

gram :: Par Setup
gram = do
  key "Time:"; ws0; xs <- separated ws1 int; nl
  key "Distance:"; ws0; ys <- separated ws1 int; nl
  pure (zipWith Race xs ys)
