module Day9 (main) where

import Misc (check)
import Par4 (Par,parse,separated,int,nl,sp,alts,lit)

main :: IO ()
main = do
  let load x = parse gram <$> readFile x
  sam <- load "../input/day9-sample.input"
  inp <-  load "../input/day9.input"
  print ("day9, part1 (sam)", check 114 $ part1 sam)
  print ("day9, part1", check 1581679977 $ part1 inp)
  print ("day9, part2 (sam)", check 2 $ part2 sam)
  print ("day9, part2", check 889 $ part2 inp)

type Line = [Int]
gram :: Par [Line]
gram = separated nl (separated sp int')
  where int' = alts [int, do lit '-'; (0-) <$> int]

part1 :: [Line] -> Int
part1 = sum . map next
  where
    next :: [Int] -> Int
    next xs = do
      if all (==0) xs then 0 else do
        last xs + next [ b - a | (a,b) <- zip xs (tail xs) ]

part2 :: [Line] -> Int
part2 = sum . map prev
  where
    prev :: [Int] -> Int
    prev xs = do
      if all (==0) xs then 0 else do
        head xs - prev [ b - a | (a,b) <- zip xs (tail xs) ]
