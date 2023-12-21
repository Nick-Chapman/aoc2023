
module Day21 (main) where

import Set (Set)
import Misc (check,the)
import Par4 (Par,parse,separated,nl,many,dot)
import qualified Set

gram :: Par GardenMap
gram = do
  grid <- separated nl (many dot)
  let h = length grid
  let w = length (head grid)
  let qs = [ ((x,y),char)
           | (y, line) <- zip [0::Int .. ] grid
           , (x, char) <- zip [0::Int .. ] line
           ]
  let start = the [ pos | (pos,'S') <- qs ]
  let rocks = Set.fromList [ pos | (pos,'#') <- qs ]
  pure $ GardenMap { w, h, rocks, start }

data GardenMap = GardenMap { w :: Int, h :: Int, rocks :: Set Pos,  start :: Pos }
  deriving Show
type Pos = (Int,Int)

main :: IO ()
main = do
  let load x = parse gram <$> readFile x
  sam <- load "../input/day21-sample.input"
  inp <- load "../input/day21.input"

  s6 <- walk 6 sam
  print ("day21, part1 (sam)", check 16 $ s6)
  i64 <- walk 64 inp
  print ("day21, part1", check 3841 $ i64)

  x <- walk 500 sam
  print (check 167004 x)

  y <- walk 26501365 inp -- hmm, dont think this will complete!
  print y

  pure ()

walk :: Int -> GardenMap -> IO Int
walk n GardenMap {w,h,rocks,start} = do
  loop 0 (0,1) Set.empty (Set.singleton start)

  where
    rocky (x,y) = (x `mod` w, y `mod` h) `Set.member` rocks

    loop :: Int -> (Int,Int) -> Set Pos -> Set Pos -> IO Int
    loop i (c1,c2) last this = do
      if i `mod` 100 == 0 && i >0 then print (i,(c1,c2)) else pure ()
      if i == n then pure c2 else do
        let next = Set.fromList
              [ p2
              | p <- Set.toList this
              , p2 <- step p
              , not (rocky p2)
              , not (p2 `Set.member` last)
              ]
        loop (i+1) (c2,c1+Set.size next) this next

step :: Pos -> [Pos]
step (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
