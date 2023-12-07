module Day7 (main) where

import Data.List (sortBy,sort)
import Data.Ord (comparing)
import Misc (check,hist)
import Par4 (Par,parse,separated,nl,int,sp,many,alts,digit,lit)
import qualified Data.Map as Map

main :: IO ()
main = do
  s <- readFile "../input/day7-sample.input"
  i <- readFile "../input/day7.input"
  print ("day7, part1 (sam)", check 6440 $ solve s Part1)
  print ("day7, part1", check 251106089 $ solve i Part1)
  print ("day7, part2 (sam)", check 5905 $ solve s Part2)
  print ("day7, part2", check 249620106 $ solve i Part2)

solve :: String -> Part -> Int
solve str part = do
  let xs = parse (gram part) str
  sum [ bid * rank
      | ((_,bid),rank) <- zip (sortBy (comparing fst) xs) [1::Int ..]
      ]

data Part = Part1 | Part2
type Setup = [Line]
type Line = (Hand,Int)
type Hand = (Type,[Int])
data Type = High | Pair | TwoPair | Three | FullHouse | Four | Five deriving (Eq,Ord,Show)

gram :: Part -> Par Setup
gram part = separated nl line
  where
    line = do xs <- many card; sp; i <- int; pure ((typeCards xs, xs), i)
    card = do
      alts [ digit
           , do lit 'T'; pure 10
           , do lit 'J'; pure (case part of Part1 -> 11; Part2 -> 1)
           , do lit 'Q'; pure 12
           , do lit 'K'; pure 13
           , do lit 'A'; pure 14 ]

typeCards :: [Int] -> Type
typeCards xs = maximum [ typeCards1 ys | ys <- expandWild xs ]

expandWild :: [Int] -> [[Int]]
expandWild = \case
  [] -> [[]]
  1:xs -> [ w:ys | ys <- expandWild xs, w <- [2..10]++[12..14] ]
  x:xs -> [ x:ys | ys <- expandWild xs ]

typeCards1 :: [Int] -> Type
typeCards1 xs =
  case sort (Map.elems (hist xs)) of
    [5] -> Five
    [1,4] -> Four
    [2,3] -> FullHouse
    [1,1,3] -> Three
    [1,2,2] -> TwoPair
    [1,1,1,2] -> Pair
    [1,1,1,1,1] -> High
    ys -> error (show ("typeCards",xs,ys))
