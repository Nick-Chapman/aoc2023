module Day1 (main) where

import Misc (check)
import Data.List as List (sort)
import Par4 (Par,parse,many,separated,nl,int)

main :: IO ()
main = do
  s <- readFile "../input/day1.input"
  let inp = parse gram s
  print ("day1, part1", check 71506 $ part1 inp)
  print ("day1, part2", check 209603 $ part2 inp)

type Setup = [[Int]]

gram :: Par Setup
gram = separated nl chunk
  where
    chunk = many num
    num = do x <- int; nl; pure x

part1 :: Setup -> Int
part1 = maximum . map sum

part2 :: Setup -> Int
part2 = sum . take 3 . reverse . List.sort . map sum
