module Day4 (main) where

import Misc (check)
import Par4 (Par,parse,separated,nl,key,ws1,int,terminated)
import Data.Set as Set (size,fromList,intersection)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "../input/day4-sample.input"
  inp <- parse gram <$> readFile "../input/day4.input"
  print ("day4, part1 (sam)", check 13 $ part1 sam)
  print ("day4, part1", check 26914 $ part1 inp)
  print ("day4, part2 (sam)", check 30 $ part2 sam)
  print ("day4, part2", check 13080971 $ part2 inp)

data Card = Card [Int] [Int]

gram :: Par [Card]
gram = separated nl card
  where
    card :: Par Card
    card = do
      key "Card"
      ws1
      _n <- int
      key ":"
      ws1
      win <- terminated ws1 int
      key "|"
      ws1
      got <- separated ws1 int
      pure (Card win got)

matches :: Card -> Int
matches (Card win got) = size (fromList win `intersection` fromList got)

part1 :: [Card] -> Int
part1 = sum . map (score . matches)
  where
    score n =
      if n == 0 then 0 else 2 ^ (n-1)

part2 :: [Card] -> Int
part2 = combine 0 (repeat 1) . map matches
  where
    combine :: Int -> [Int] -> [Int] -> Int
    combine acc ns xs = do
      case (ns,xs) of
        (n:ns,x:xs) -> do
          let (as,bs) = (take x ns,drop x ns)
          let transformed = map (+n) as ++ bs
          combine (acc+n) transformed xs
        _ -> acc
