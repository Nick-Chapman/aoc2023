{-# LANGUAGE DeriveAnyClass #-}

module Day16 (main) where

import Data.Array (Array,array,(!))
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Misc (check)
import Par4 (Par,parse,separated,many,lit,alts,nl)
import qualified Data.HashSet as Set

type Set = HashSet

gram :: Par Grid
gram = separated nl (many cell)
  where
    cell = alts [ do lit '.'; pure E
                , do lit '-'; pure H
                , do lit '|'; pure V
                , do lit '/'; pure A
                , do lit '\\'; pure G ]

type Grid = [[Cell]]
data Cell = E | V | H | A | G deriving Show

main :: IO ()
main = do
  let load x = parse gram <$> readFile x
  sam <- load "../input/day16-sample.input"
  inp <- load "../input/day16.input"
  print ("day16, part1 (sam)", check 46 $ part1 sam)
  print ("day16, part1", check 8112 $ part1 inp)
  print ("day16, part2 (sam)", check 51 $ maximum (part2 sam))
  let res = part2 inp
  --mapM_ print res
  print ("day16, part2 (sam)", check 8314 $ maximum res)

part1 :: Grid -> Int
part1 grid = countA grid ((1,1),R)

part2 :: Grid -> [Int]
part2 grid = do
  let
    h = length grid
    w = length (head grid)
  let
    allBeams = [ ((1,y),R) | y <- [1::Int .. h] ] ++
               [ ((w,y),L) | y <- [1::Int .. h] ] ++
               [ ((x,1),D) | x <- [1::Int .. w] ] ++
               [ ((x,h),U) | x <- [1::Int .. w] ]
  [ countA grid b | b <- allBeams ]

countA :: Grid -> Beam -> Int
countA grid b = do
  Set.size (activated (shine Set.empty b))
  where
    step = induce grid
    shine :: Set Beam -> Beam -> Set Beam
    shine acc b = do
      if b `Set.member` acc then acc else do
        foldl shine (Set.insert b acc) (step b)

activated :: Set Beam -> Set Pos
activated set = Set.fromList [ p | (p,_) <- Set.toList set ]

type Beam = (Pos,Dir)
type Pos = (Int,Int)
data Dir = L | R | U | D deriving (Show,Eq,Ord,Hashable,Generic)

induce :: Grid -> (Beam -> [Beam])
induce grid = do
  let
    h = length grid
    w = length (head grid)
  let
    a :: Array Pos Cell
    a = array ((1,1),(w,h))
        [ ((x,y),cell) | (y,line) <- zip [1::Int ..] grid
                       , (x,cell) <- zip [1::Int ..] line
                       ]
  let onBoard (x,y) = x>=1 && x<=w && y>=1 && y<=h
  let get p = a!p
  \(p,d) ->
    [ (p', d') | d' <- bend (get p) d, let p'= adj p d', onBoard p' ]

bend :: Cell -> Dir -> [Dir]
bend = \case
  E -> \x -> [x]
  A -> \case L -> [D]; R -> [U]; U -> [R]; D -> [L]
  G -> \case L -> [U]; R -> [D]; U -> [L]; D -> [R]
  H -> \case L -> [L]; R -> [R]; U -> [L,R]; D -> [L,R]
  V -> \case L -> [U,D]; R -> [U,D]; U -> [U]; D -> [D]

adj :: Pos -> Dir -> Pos
adj (x,y) = \case L -> (x-1,y); R -> (x+1,y); U -> (x,y-1); D -> (x,y+1)
