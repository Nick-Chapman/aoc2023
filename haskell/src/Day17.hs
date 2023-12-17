{-# LANGUAGE DeriveAnyClass #-}

module Day17 (main) where

import Data.Array (Array,array,(!))
import Misc (check)
import Par4 (Par,parse,separated,many,nl,digit)
import Control.Monad (when)
import GHC.Generics (Generic)
import Data.Hashable (Hashable)

-- Order based sets:
import Set (Set)
import qualified Set

-- Hash based sets: (very slightly slower!)
-- import Data.HashSet (HashSet)
-- import qualified Data.HashSet as Set
-- type Set = HashSet


gram :: Par Grid
gram = separated nl (many digit)

type Grid = [[Int]]

main :: IO ()
main = do
  let load x = parse gram <$> readFile x
  sam <- load "../input/day17-sample.input"
  _inp <- load "../input/day17.input"

  sam1 <- explore Part1 sam
  print ("day16, part1 (sam)", check 102 $ sam1)

  inp1 <- explore Part1 _inp -- 12s
  print ("day16, part1", check 817 $ inp1)

  sam2 <- explore Part2 sam
  print ("day16, part2 (sam)", check 94 $ sam2)

  --inp2 <- explore Part2 _inp -- 138s
  --print ("day16, part2", check 925 $ inp2)


data Part = Part1 | Part2
balanceParams :: Part -> (Int,Int)
balanceParams = \case Part1 -> (1,3); Part2 -> (4,10)


explore :: Part -> Grid -> IO Int
explore part grid = loop 1 states
  where
    endPos = sizeGrid grid
    states = searchGrid part grid

    loop :: Int -> [State] -> IO Int
    loop i = \case
      [] -> error "no-more-states"
      State{q}:states -> do
        when (i `mod` 10000 == 0) $ print (i,head q)
        case q of
          [] -> error "empty-q"
          (res,(pos,_)):_ -> do
            if pos /= endPos then loop (i+1) states else do
              print i
              pure res


sizeGrid :: Grid -> Pos
sizeGrid grid = (w,h)
  where
    h = length grid
    w = length (head grid)

searchGrid :: Part -> Grid -> [State]
searchGrid part grid = do
  let endPos@(w,h) = sizeGrid grid
  let a :: Array Pos Int
      a = array ((1,1),(w,h))
        [ ((x,y),cell) | (y,line) <- zip [1::Int ..] grid
                       , (x,cell) <- zip [1::Int ..] line ]
  let cost :: Pos -> Int = (a!)
  let ns0 :: [Node] = [ ((1,1),R), ((1,1),D) ]
  let step :: Node -> [(Node,Cost)] = mkStep part endPos cost
  search step (state0 ns0)

mkStep :: Part -> Pos -> (Pos -> Int) -> (Node -> [(Node,Cost)])
mkStep part pMax costF = do
  let
    (a,b) = balanceParams part
    step node@(pos,dir) = do
      [ ((pos',dir'), cost)
        | n <- [a::Int .. min b (maxStep pMax node) ]
        , let (cover,pos') = stride (adj pos dir) (n-1) dir []
        , let cost = sum (map costF cover) -- TODO: share sum over strides
        , dir' <- turns dir
        ]
  step

turns :: Dir -> [Dir]
turns = \case U -> [L,R]; D -> [L,R]; L -> [U,D]; R -> [U,D]

maxStep :: Pos -> Node -> Int
maxStep (w,h) ((x,y),dir) = case dir of U -> y-1; D -> h-y; L -> x-1; R -> w-x

stride :: Pos -> Int -> Dir -> [Pos] -> ([Pos],Pos)
stride p n dir acc =
  if n == 0 then (p:acc,p) else
    stride (adj p dir) (n-1) dir (p:acc)

adj :: Pos -> Dir -> Pos
adj (x,y) = \case L -> (x-1,y); R -> (x+1,y); U -> (x,y-1); D -> (x,y+1)

--newtype Cost = Cost Int deriving (Show,Num,Eq,Ord)
type Cost = Int

data Dir = U | D | L | R deriving (Eq,Ord,Show,Hashable,Generic)
type Pos = (Int,Int)
type Node = (Pos,Dir)

data State = State { v :: Set Node, q :: Q }
type Q = [ (Cost,Node) ]

pushQ :: Q -> (Cost,Node) -> Q
pushQ q (c,n) =
  case q of
    [] -> [(c,n)]
    x@(c1,n1):q' ->
      if c <= c1 then (c,n):q else
        if n==n1 then q else
          x : pushQ q' (c,n)

state0 :: [Node] -> State
state0 ns = State { v = Set.empty, q = [ (0, n) | n <- ns ] }

search :: (Node -> [(Node,Cost)]) -> State -> [State]
search step = loop
  where
    loop :: State -> [State]
    loop s@State{v,q} = s :
      case q of
        [] -> []
        (c1,nodeSelected):q -> do
          if nodeSelected `Set.member` v then loop s { q } else do
            let q' = foldl pushQ q
                  [ ( c1+c2, n')
                  | (n',c2) <- step nodeSelected
                  , not (n' `Set.member` v)
                  ]
            loop State { v = Set.insert nodeSelected v, q = q' }
