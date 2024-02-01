
module Day23 (main) where

import Data.Maybe (maybeToList) --,catMaybes)
import Data.Set (Set)
import Data.Map (Map)
import Misc (check)
import Par4 (Par,parse,separated,nl,lit,many,alts)
import qualified Data.Map as Map
import qualified Data.Set as Set

gram :: Par Maze
gram = separated nl (many cell)
  where
    cell :: Par Cell
    cell = alts [ do lit '.'; pure Dot
                , do lit '#'; pure Hash
                , do lit '<'; pure Langle
                , do lit '>'; pure Rangle
                , do lit '^'; pure Hat
                , do lit 'v'; pure Vee ]

type Maze = [[Cell]]
data Cell = Dot | Hash | Langle | Rangle | Hat | Vee deriving Show

main :: IO ()
main = do
  let load x = parse gram <$> readFile x
  _sam <- load "../input/day23-sample.input"
  _inp <- load "../input/day23.input"
  print ("day23, part1 (sam)", check 94 $ part1 _sam)
  print ("day23, part1", check 2018 $ part1 _inp)

type Pos = (Int,Int)
data Dir = L | R | U | D deriving (Eq,Show)

part1 :: Maze -> Int
part1 maze = do
  let h = length maze
  let w = length (head maze)
  let stop p = p == (w-1,h)
  let
    at :: Pos -> Cell
    at = flip look m
      where m = Map.fromList
              [ ((x,y),cell) | (y,line) <- zip [1..] maze , (x,cell) <- zip [1..] line ]
  let
    walk :: (Dir,Pos) -> Int
    walk dp = do
      let (i,opt) = walkToJunction stop at dp
      case opt of
        Nothing -> i
        Just dps -> i + maximum (map walk dps)
  let start :: Pos = (2,1)
  walk (D,start)

walkToJunction :: (Pos -> Bool) -> (Pos -> Cell) -> (Dir,Pos) -> (Int,Maybe [(Dir,Pos)])
walkToJunction stop at = loop 0
  where
    loop i (dirLast,pos) = if stop pos then (i,Nothing) else do
      let
        alts :: [(Dir,Pos)]
        alts =
          [ (dir,pos') | dir <- allWays
                       , dir /= opp dirLast
                       , let pos' = adj pos dir
                       , let cell = at pos'
                       , canStep cell dir
                       ]
      case alts of
        [] -> error (show ("no alts",dirLast,pos))
        [dp] -> loop (i+1) dp
        _ -> (i+1,Just alts)

allWays :: [Dir]
allWays = [L,R,U,D]

adj :: Pos -> Dir -> Pos
adj (x,y) = \case L -> (x-1,y); R -> (x+1,y); U -> (x,y-1); D -> (x,y+1)

opp :: Dir -> Dir
opp = \case L -> R; R -> L; U -> D; D -> U

canStep :: Cell -> Dir -> Bool
canStep = \case
  Dot -> \_ -> True
  Hash -> \_ -> False
  Langle -> \case L -> True; _ -> False
  Rangle -> \case R -> True; _ -> False
  Hat -> \case U -> True; _ -> False
  Vee -> \case D -> True; _ -> False
