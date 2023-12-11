module Day11 (main) where

import Misc (pairwise,check)
import Par4 (Par,parse,separated,nl,many,dot)

main :: IO ()
main = do
  let load x = parse gram <$> readFile x
  sam <- load "../input/day11-sample.input"
  inp <- load "../input/day11.input"
  print ("day11, part1 (sam)", check 374 $ solve 1 sam)
  print ("day11, part1", check 9805264 $ solve 1 inp)
  print ("day11, part2 (sam)", check 8410 $ solve 99 sam)
  print ("day11, part2", check 779032247216 $ solve 999999 inp)

gram :: Par [[Char]]
gram = separated nl (many dot)

type Pos = (Int,Int)

setup :: [[Char]] -> [Pos]
setup grid = [ (x,y) | (y,line) <- zip [1..] grid , (x,'#') <- zip [1..] line ]

solve :: Int -> [[Char]] -> Int
solve m grid = do
  let gs = setup grid
  let (ymax,xmax) = (length grid, length (head grid))
  let vblank = [ x | x <- [1..xmax] , null [ () | y <- [1..ymax], (x,y) `elem` gs ] ]
  let hblank = [ y | y <- [1..ymax] , null [ () | x <- [1..xmax], (x,y) `elem` gs ] ]
  let
    dist (x1,y1) (x2,y2) =
      let (xlo,xhi) = (min x1 x2, max x1 x2) in
      let (ylo,yhi) = (min y1 y2, max y1 y2) in
      (xhi-xlo + m * length [ x | x <- vblank, xlo<x && x<xhi ]) +
      (yhi-ylo + m * length [ y | y <- hblank, ylo<y && y<yhi ])

  sum [ dist g1 g2 | (g1,g2) <- pairwise gs ]
