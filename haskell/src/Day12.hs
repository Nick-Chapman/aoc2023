module Day12 (main) where

import Data.Array (Array,array,(!))
import Data.List (intercalate)
import Misc (check)
import Par4 (Par,parse,separated,nl,many,sp,lit,int,alts)

gram :: Par [Line]
gram = separated nl line
  where
    line = do
      xs <- many cell
      sp
      ns <- separated (lit ',') int
      pure (Line xs ns)
    cell = alts [ do lit '.'; pure Dot
                , do lit '#'; pure Hash
                , do lit '?'; pure Query ]

data Line = Line [Cell] [Int] deriving Show
data Cell = Dot | Hash | Query deriving (Eq,Show)

main :: IO ()
main = do
  let load x = parse gram <$> readFile x
  sam <- load "../input/day12-sample1.input"
  inp <- load "../input/day12.input"
  print ("day12, part1 (sam)", check [1,4,1,1,4,10] $ part1 sam)
  print ("day12, part1", check 7843 $ sum (part1 inp))
  print ("day12, part2 (sam)", check [1,16384,1,16,2500,506250] $ part2 sam)
  print ("day12, part2", check 10153896718999 $ sum (part2 inp))

part1 :: [Line] -> [Int]
part1 = map countFast

part2 :: [Line] -> [Int]
part2 = map (countFast . rep5)

rep5 :: Line -> Line
rep5 (Line xs ns) = Line xs' ns'
  where
    xs' = intercalate [Query] (take 5 (repeat xs))
    ns' = concat (take 5 (repeat ns))

countFast :: Line -> Int
countFast (Line xs ns) = lookup 0 0 0
  where
    -- x: index into cells list
    -- n: index into counts (of contiguous damaged) list
    -- k: last N when we saw a # (reduced by 1 for each following #), or 0 if we last saw a dot

    xMax = length xs
    nMax = length ns
    kMax = maximum ns

    a :: Array (Int,Int,Int) Int
    a = array ((0,0,0),(xMax,nMax,kMax)) [ ((x,n,k), compute x n k)
                                         | x <- [0..xMax] , n <- [0..nMax] , k <- [0..kMax]
                                         ]
    lookup :: Int -> Int -> Int -> Int
    lookup x n k = a!(x,n,k)

    compute :: Int -> Int -> Int -> Int
    compute x n k =
      if | x == xMax ->
            if | k==0 -> if n==nMax then 1 else 0
               | otherwise -> if n==nMax && k==1 then 1 else 0
         | otherwise -> do
             let dot =
                   if | k==0 -> lookup (x+1) n 0
                      | otherwise -> if k==1 then lookup (x+1) n 0 else 0
             let hash =
                   if | k==0 -> if n==nMax then 0 else lookup (x+1) (n+1) (ns!!n)
                      | otherwise -> if k==1 then 0 else lookup (x+1) n (k-1)
             case xs!!x of
               Query -> dot + hash
               Dot -> dot
               Hash -> hash
