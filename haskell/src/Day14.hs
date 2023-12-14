module Day14 (main) where

import Data.List (transpose)
import Data.Map (Map)
import Misc (check)
import Par4 (Par,parse,separated,nl,many,alts,lit)
import qualified Data.Map as Map

type Grid = [[Cell]]
data Cell = Gap | Round | Square deriving (Eq,Ord)

gram :: Par Grid
gram = separated nl (many cell)
  where
    cell = alts [ do lit '.'; pure Gap
                , do lit 'O'; pure Round
                , do lit '#'; pure Square ]

main :: IO ()
main = do
  let load x = parse gram <$> readFile x
  sam <- load "../input/day14-sample.input"
  inp <- load "../input/day14.input"
  print ("day14, part1 (sam)", check 136 $ part1 sam)
  print ("day14, part1", check 105208 $ part1 inp)
  print ("day14, part2 (sam)", check 64 $ part2 sam)
  print ("day14, part2", check 102943 $ part2 inp)

part1 :: Grid -> Int
part1 = weigh . north

part2 :: Grid -> Int
part2 grid = do
  let gs = iterate (east . south . west . north) grid
  let (i,n) = measureLoop 0 Map.empty gs
  let billion = 1000000000::Int
  let pick = i + ((billion - i) `mod` (n-i))
  weigh (gs !! pick)

measureLoop :: Ord a => Int -> Map a Int -> [a] -> (Int,Int)
measureLoop n m = \case
  [] -> error "measureLoop[]"
  x:xs ->
    case Map.lookup x m of
      Just i -> (i,n)
      Nothing -> measureLoop (n+1) (Map.insert x n m) xs

weigh :: Grid -> Int
weigh = sum . map weighLine . transpose
  where
    weighLine :: [Cell] -> Int
    weighLine xs = sum [ n | (Round,n) <- zip (reverse xs) [1::Int ..] ]

north :: Grid -> Grid
north = transpose . map tilt . transpose

south :: Grid -> Grid
south = transpose . map (reverse . tilt . reverse) . transpose

west :: Grid -> Grid
west = map tilt

east :: Grid -> Grid
east = map (reverse . tilt . reverse)

tilt :: [Cell] -> [Cell]
tilt = loop 0 0
  where
    loop :: Int -> Int -> [Cell] -> [Cell]
    loop g r = \case
      [] -> fill
      Gap:xs -> loop (g+1) r xs
      Round:xs -> loop g (r+1) xs
      Square:xs -> fill ++ [Square] ++ loop 0 0 xs
      where fill = replicate r Round ++ replicate g Gap
