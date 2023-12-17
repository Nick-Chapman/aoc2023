
module Day17 (main) where

import Data.Array (Array,array,(!))
import Misc (check)
import Par4 (Par,parse,separated,many,nl,digit)
import Set (Set)
import qualified Set -- TODO: HashSet is quicker?

gram :: Par Grid
gram = separated nl (many digit)

type Grid = [[Int]]

main :: IO ()
main = do
  let load x = parse gram <$> readFile x
  sam <- load "../input/day17-sample.input"
  _inp <- load "../input/day17.input"

  sam1 <- explore Part1 sam
  --inp1 <- explore Part1 _inp -- approx 90s
  sam2 <- explore Part2 sam
  --inp2 <- explore Part2 _inp -- ages! ~ 40 minutes ?

  print ("day16, part1 (sam)", check 102 $ sam1)
  --print ("day16, part1", check 817 $ inp1)
  print ("day16, part2 (sam)", check 94 $ sam2)
  --print ("day16, part2", check 925 $ part2 inp)

  pure ()

explore :: Part -> Grid -> IO Int
explore part grid = do
  let h = length grid
  let w = length (head grid)
  let a :: Array Pos Int
      a = array ((1,1),(w,h))
        [ ((x,y),cell) | (y,line) <- zip [1::Int ..] grid
                       , (x,cell) <- zip [1::Int ..] line ]
  let cost :: Pos -> Int = (a!)
  let ns0 :: [Node] = [ ((1,1),R), ((1,1),D) ]
  let step :: Node -> [(Node,Cost)] = mkStep part (w,h) cost
  let res = search step (state0 ns0)
  let isDone :: Pos -> Bool
      isDone (x,y) = x == w && y == h
  let f :: State -> Bool
      f State{q} = case q of [] -> False; (_,(pos,_)):_ -> not (isDone pos)
  let (_pre,post) = span f res
  -- mapM_ print $ zip [1::Int ..] [ x | State{q=x:_} <- _pre]
  -- mapM_ print $ zip [1::Int ..] [ x | State{q=x:_} <- take 5 post]
  let State{q} = head post
  let x = head q
  let (Cost res,_) = x
  pure res

data Part = Part1 | Part2

mkStep :: Part -> Pos -> (Pos -> Int) -> (Node -> [(Node,Cost)])
mkStep part pMax costF = do
  let
    (a,b) = case part of Part1 -> (1,3); Part2 -> (4,10)
    step node@(pos,dir) = do
      [ ((pos',dir'), cost)
        | n <- [a::Int .. min b (maxStep pMax node) ]
        , let (cover,pos') = stride (adj pos dir) (n-1) dir []
        , let cost = Cost $ sum (map costF cover) -- TODO: share sum over strides
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

newtype Cost = Cost Int deriving (Show,Num,Eq,Ord)

data Dir = U | D | L | R deriving (Eq,Ord,Show)
type Pos = (Int,Int)
type Node = (Pos,Dir)

data State = State { v :: Set Node, q :: Q }
type Q = [ (Cost,Node) ]

pushQ :: Q -> (Cost,Node) -> Q
pushQ q (c,n) =
  case q of
    [] -> [(c,n)]
    x@(c1,_):q' ->
      -- TODO: drop node when already exists with a lower cost
      if c <= c1 then (c,n):q else
        x : pushQ q' (c,n)

state0 :: [Node] -> State
state0 ns = State { v = Set.empty, q = [ (Cost 0, n) | n <- ns ] }

search :: (Node -> [(Node,Cost)]) -> State -> [State]
search step = loop
  where
    loop :: State -> [State]
    loop s@State{v,q} = s :
      case q of
        [] -> []
        (c1,nodeSelected):q -> do
          if nodeSelected `Set.member` v then loop s { q } else do
            -- TODO: dont push node when already visited
            let q' = foldl pushQ q [ ( c1+c2, n') | (n',c2) <- step nodeSelected ]
            loop State { v = Set.insert nodeSelected v, q = q' }
