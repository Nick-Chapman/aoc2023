module Day25 (main) where

import Data.List (maximumBy)
import Data.Map as Map (toList)
import Data.Ord (comparing)
import Data.Set (Set,(\\))
import Misc (check,nub,hist)
import Par4 (Par,parse,terminated,separated,nl,key,lit,word)
import qualified Data.Set as Set

gram :: Par Graph
gram = (Graph . concat) <$> terminated nl line
  where
    line :: Par [Edge]
    line = do
      n1 <- node
      key ": "
      ns <- separated (lit ' ') node
      return [ (n1,n2) | n2 <- ns ]
    node = word

data Graph = Graph [Edge] deriving Show
type Edge = (Node,Node)
type Node = String

main :: IO ()
main = do
  let load x = parse gram <$> readFile x

  sam <- load "../input/day25-sample.input"
  (a,b) <- explore sam
  print ("day25, part1 (sample)", check (a, b, a*b) (6, 9 ,54))

  inp <- load "../input/day25.input"
  (a,b) <- explore inp
  print ("day25, part1", check (a, b, a*b) (773, 715, 552695))
  pure ()

explore :: Graph -> IO (Int,Int)
explore g0 = loop z0 g0
  where
    z0 = length (nodesOf g0)
    loop :: Int -> Graph -> IO (Int,Int)
    loop z g = do
      print z
      if z == 1 then undefined else do
        (a,b,n) <- phase g
        print (a,b,n)
        let g' = mergeNodes a b g
        let z' = z-1
        if n == 3 then do pure (z', z0-z') else loop z' g'

mergeNodes :: Node -> Node -> Graph -> Graph
mergeNodes a b (Graph es) = Graph es'
  where
    ab = a ++ "-" ++ b
    es' = [ (s',d')
          | (s,d) <- es
          , let s' = if s == a || s == b then ab else s
          , let d' = if d == a || d == b then ab else d
          ]

phase :: Graph -> IO (Node,Node,Int)
phase g = loop 0 picked0 src0 dest0
  where
    all = Set.fromList (nodesOf g)
    picked0 = pickNode g
    src0 = Set.singleton picked0
    dest0 = all \\ src0

    loop :: Int -> Node -> Set Node -> Set Node -> IO (Node,Node,Int)
    loop i pen src dest = do
      let xs = cheaper g src dest
      let m = maximumBy (comparing snd) xs
      let (node,n) = m
      let src' = Set.insert node src
      let dest' = Set.delete node dest
      if Set.null dest'
      then pure (pen,node,n)
      else loop (i+1) node src' dest'

pickNode :: Graph -> Node
pickNode (Graph es) =
  case es of [] -> undefined; (n,_):_ -> n

nodesOf :: Graph -> [Node]
nodesOf (Graph es) =
  nub [ n | (n1,n2) <- es, n <- [n1,n2] ]

cheaper :: Graph -> Set Node -> Set Node ->  [(Node,Int)]
cheaper (Graph es) src _dest = do
  let xs = [ d | (s,d) <- es , s `Set.member` src, not (d `Set.member` src) ]
  let ys = [ d | (d,s) <- es , s `Set.member` src, not (d `Set.member` src) ]
  Map.toList $ hist (xs ++ ys)
