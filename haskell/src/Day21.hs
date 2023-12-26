
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

  s2a <- calc 500 sam
  print ("day21, part2 (500)", check 167004 $ s2a)
  s2b <- calc 1000 sam
  print ("day21, part2 (1000)", check 668697 $ s2b)
  s2c <- calc 5000 sam
  print ("day21, part2 (5000)", check 16733044 $ s2c)

  res <- calc 26501365 inp
  print ("day21, part2", check 636391426712747 $ res)


calc :: Int -> GardenMap -> IO Int
calc n gm@GardenMap {w,h} = do
  --print ("n",n)
  if w /= h then error "w/=h" else do
    let r = 4*w
    --print ("r",r)
    let (d,m) = (n `div` r, n `mod` r)
    --print ("d/m",d,m)
    let (i1,i2,i3,_i4) = (m+r, m+2*r, m+3*r, m+4*r)
    --print ("i:",i1,i2,i3,i4)
    w1 <- walk i1 gm
    w2 <- walk i2 gm
    w3 <- walk i3 gm
    --w4 <- walk _i4 gm
    let w4 = 0
    --print ("w:",w1,w2,w3,w4)
    let (x1,x2,x3) = (w2-w1,w3-w2,w4-w3)
    --print ("x",x1,x2,x3)
    let (y1,_y2) = (x2-x1,x3-x2)
    --if y1 /= _y2 then error "y1/=y2" else do
    --print ("y",y1,y2)
    --print (m+d*r)
    let res = w1 + (d-1)*x1 + ((d-2)*(d-1) `div` 2) * y1
    pure res


walk :: Int -> GardenMap -> IO Int
walk n GardenMap {w,h,rocks,start} = do
  loop 0 (0,1) Set.empty (Set.singleton start)

  where
    rocky (x,y) = (x `mod` w, y `mod` h) `Set.member` rocks

    loop :: Int -> (Int,Int) -> Set Pos -> Set Pos -> IO Int
    loop i (c1,c2) last this = do
      --if i `mod` 1000 == 0 && i >0 then print (i,(c1,c2)) else pure ()
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
