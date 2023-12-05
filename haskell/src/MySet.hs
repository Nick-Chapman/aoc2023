
module MySet
  ( Set(..)
  , fromList, toList
  , size, null, member, notMember
  , empty, singleton, insert, delete
  , map
  , union, intersection, difference
  ) where

import Prelude hiding (null,map)
import qualified Prelude
import Data.List (intercalate)

instance Show a => Show (Set a) where
  show set = "{" ++ (intercalate "," . Prelude.map show . toList) set ++ "}"

instance Foldable Set where
  foldr f b set = foldr f b (toList set)

map :: Ord b => (a -> b) -> Set a -> Set b
map f = fromList . Prelude.map f . toList

fromList :: Ord a => [a] -> Set a
fromList = foldr insert empty

data Set a
  = Empty
  | Node { left :: Set a
         , elem :: a
         , right :: Set a
         , sizeNode :: Int
         , heightNode :: Int
         }
    deriving (Eq,Ord)

null :: Set a -> Bool
null = \case
  Empty{} -> True
  Node{} -> False

empty :: Set a
empty = Empty

singleton :: Ord a => a -> Set a
singleton x = nodeSH empty x empty

toList :: Set a -> [a]
toList = walk []
  where
    walk :: [a] -> Set a -> [a]
    walk after = \case
      Empty -> after
      Node{left=l,elem=e,right=r} ->
        walk (e : walk after r) l

size :: Set a -> Int
size = \case
  Empty -> 0
  Node{sizeNode=x} -> x

height :: Set a -> Int
height = \case
  Empty -> 0
  Node{heightNode=x} -> x

notMember :: Ord a => a -> Set a -> Bool
notMember x = not . member x

{-
member :: Ord a => a -> Set a -> Bool
member x s = let (_,b,_) = splitOn x s in b

insert :: Ord a => a -> Set a -> Set a
insert x set = singleton x `union` set

delete :: Ord a => a -> Set a -> Set a
delete x set = set `difference` singleton x
-}

member :: Ord a => a -> Set a -> Bool
member x = \case
  Empty -> False
  Node{left=l,elem=e,right=r} ->
    x == e || member x (if x < e then l else r)

insert :: Ord a => a -> Set a -> Set a
insert x = \case
  Empty -> singleton x
  n@Node{left=l,elem=e,right=r} -> if x == e then n else
    if x < e then mkNode (insert x l) e r else
    mkNode l e (insert x r)

delete :: Ord a => a -> Set a -> Set a
delete x = \case
  Empty -> Empty
  Node {left=l,elem=e,right=r} ->
    if x < e then mkNode (delete x l) e r else
    if x > e then mkNode l e (delete x r) else
    abut (l,r)

union :: Ord a => Set a -> Set a -> Set a
union s1 s2 =
  case s1 of
    Empty{} -> s2
    Node{left=l1,elem=e1,right=r1} ->
      case s2 of
        Empty -> s1
        Node{} -> do
          let (l2,_b2,r2) = splitOn e1 s2
          mkNode (union l1 l2) e1 (union r1 r2)

intersection :: Ord a => Set a -> Set a -> Set a
intersection s1 s2 =
  case s1 of
    Empty{} -> empty
    Node{left=l1,elem=e1,right=r1} ->
      case s2 of
        Empty -> empty
        Node{} -> do
          let (l2,b2,r2) = splitOn e1 s2
          if b2
          then mkNode (intersection l1 l2) e1 (intersection r1 r2)
          else abut (intersection l1 l2, intersection r1 r2)

difference :: Ord a => Set a -> Set a -> Set a
difference s1 s2 =
  case s1 of
    Empty{} -> empty
    Node{left=l1,elem=e1,right=r1} ->
      case s2 of
        Empty -> s1
        Node{} -> do
          let (l2,b2,r2) = splitOn e1 s2
          if b2
          then abut (difference l1 l2, difference r1 r2)
          else mkNode (difference l1 l2) e1 (difference r1 r2)

splitOn :: Ord a => a -> Set a -> (Set a, Bool, Set a)
splitOn x = \case
  Empty -> (Empty,False,Empty)
  Node{left,elem,right}-> do
    if x == elem
      then (left,True,right)
      else if x < elem
           then let (l,b,r) = splitOn x left in (l, b, mkNode r elem right)
           else let (l,b,r) = splitOn x right in (mkNode left elem l, b, r)

abut :: Ord a => (Set a,Set a) -> Set a
abut = \case
  (Empty,Empty) -> Empty
  (Empty,set) -> set
  (set,Empty) -> set
  (Node{left=l1,elem=e1,right=r1}, Node{left=l2,elem=e2,right=r2}) -> do
    mkNode l1 e1 (mkNode (abut (r1,l2)) e2 r2)

-- rebalance during node construction
mkNode :: Ord a => Set a -> a -> Set a -> Set a
mkNode l e r = case (l,r) of { (Empty,Empty) -> singleton e ; _ -> do
  let u = height l - height r
  if u <=1 && u >= -1 then nodeSH l e r else
    if u < 0
    then
      case r of
        Empty -> error "impossible: height(r) >= 2"
        Node {left=rl,elem=re,right=rr} ->
          nodeSH (nodeSH l e rl) re rr
    else
      case l of
        Empty -> error "impossible: height(l) >= 2"
        Node {left=ll,elem=le,right=lr} ->
          nodeSH ll le (nodeSH lr e r) }

-- compute size & height
nodeSH :: Ord a => Set a -> a -> Set a -> Set a
nodeSH left elem right =
  Node { left
       , elem
       , right
       , sizeNode = 1 + size left + size right
       , heightNode = 1 + max (height left) (height right)
       }
