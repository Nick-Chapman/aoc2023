
module Day18 (main) where

import Misc (check,nub,look)
import Par4 (Par,parse,separated,nl,digit,sp,int,alts,lit,dot)
import Set (Set)
import qualified Data.Map as Map
import qualified Set

main :: IO ()
main = do
  let load1 x = parse gram1 <$> readFile x
  sam1 <- load1 "../input/day18-sample.input"
  inp1 <- load1 "../input/day18.input"
  print ("day18, part1 (sam)", check 62 $ compute sam1)
  print ("day18, part1", check 61865 $ compute inp1)

  let load2 x = parse gram2 <$> readFile x
  sam2 <- load2 "../input/day18-sample.input"
  inp2 <- load2 "../input/day18.input"
  print ("day18, part2 (sam)", check 952408144115 $ compute sam2)
  print ("day18, part2", check 40343619199142 $ compute inp2)

gram1 :: Par Path
gram1 = separated nl line
  where
    line :: Par Step
    line = do
      d <- dir
      sp
      i <- int
      sp
      _ <- col
      pure (d,i)
    dir = alts [ do lit 'U'; pure U
               , do lit 'D'; pure D
               , do lit 'L'; pure L
               , do lit 'R'; pure R ]
    col :: Par String
    col = do
      lit '('
      lit '#'
      s <- sequence (replicate 6 dot)
      lit ')'
      pure s

gram2 :: Par Path
gram2 = separated nl line
  where
    line :: Par Step
    line = do
      _d <- dot
      sp
      _i <- int
      sp
      lit '('
      lit '#'
      s <- sequence (replicate 5 hdigit)
      d <- dir
      lit ')'
      let i = foldl f 0 s where f acc d = 16 * acc + d
      pure (d,i)

    hdigit =
      alts [ digit
           , do lit 'a'; pure 10
           , do lit 'b'; pure 11
           , do lit 'c'; pure 12
           , do lit 'd'; pure 13
           , do lit 'e'; pure 14
           , do lit 'f'; pure 15
           ]

    dir = alts [ do lit '3'; pure U
               , do lit '1'; pure D
               , do lit '2'; pure L
               , do lit '0'; pure R ]

type Path = [Step]
type Step = (Dir,Int)
data Dir = U | D | L | R deriving Show
type Pos = (Int,Int)

compute :: Path -> Int
compute path0 = do
  let (xs0,ys0) = unzip $ corners (1,1) path0
  let xs = nub xs0 -- result of nub is sorted
  let ys = nub ys0
  let fx = \k -> 1 + 2 * look k m where m = Map.fromList (zip xs [0::Int ..])
  let fy = \k -> 1 + 2 * look k m where m = Map.fromList (zip ys [0::Int ..])
  let rebase (x,y) = (fx x, fy y)
  let path = rebasePath rebase (1,1) path0
  let pSet = Set.fromList $ perim (1,1) path
  let iSet = flood pSet Set.empty (2,2)
  let piSet = pSet `Set.union` iSet
  let wxs = computeWidths xs
  let wys = computeWidths ys
  let minX = minimum [ x | (x,_) <- Set.toList pSet ]
  let minY = minimum [ y | (_,y) <- Set.toList pSet ]
  let wx =  \k -> look k m where m = Map.fromList (zip [minX::Int ..] wxs)
  let wy =  \k -> look k m where m = Map.fromList (zip [minY::Int ..] wys)
  let area (x,y) = (wx x * wy y)
  sum [ area pos | pos <- Set.toList piSet ]

corners :: Pos -> Path -> [Pos]
corners p = \case
  [] -> []
  (dir,n):xs -> do
    let p' = adjN n p dir
    p' : corners p' xs

rebasePath :: (Pos -> Pos) -> Pos -> [Step] -> [Step]
rebasePath rebase p = \case
  [] -> []
  (dir,n):xs -> do
    let p' = adjN n p dir
    let (a,b) = rebase p
    let (c,d) = rebase p'
    let n' = abs (a-c) + abs (b-d) -- one side of the + with be 0
    (dir, n') : rebasePath rebase p' xs

computeWidths :: [Int] -> [Int]
computeWidths = \case
  [] -> undefined
  [_] -> [1]
  x:xs@(y:_) -> 1 : (y-x-1) : computeWidths xs

flood :: Set Pos -> Set Pos -> Pos -> Set Pos
flood perim = walk
  where
    walk :: Set Pos -> Pos -> Set Pos
    walk acc pos =
      if pos `Set.member` perim then acc else
        if pos `Set.member` acc then acc else
          foldl walk (Set.insert pos acc) (map (adjN 1 pos) [U,D,L,R])

perim :: Pos -> [Step] -> [Pos]
perim p = \case
  [] -> []
  (_,0):xs -> perim p xs
  (dir,n):xs -> do
    let p' = adjN 1 p dir
    p' : perim p' ((dir,n-1):xs)

adjN :: Int -> Pos -> Dir -> Pos
adjN n (x,y) = \case L -> (x-n,y); R -> (x+n,y); U -> (x,y-n); D -> (x,y+n)
