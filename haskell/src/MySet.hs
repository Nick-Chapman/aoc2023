
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

instance Show a => Show (Set a) where
  show = show . toList -- TODO: curlys

instance Foldable Set where
  foldr f b set = foldr f b (toList set)

map :: Ord b => (a -> b) -> Set a -> Set b
map f = fromList . Prelude.map f . toList

fromList :: Ord a => [a] -> Set a
fromList = foldr insert empty

union :: Ord a => Set a -> Set a -> Set a
union x y = if size x < size y then f x y else f y x
  where
    f smaller bigger =
      foldr insert bigger (toList smaller)

intersection :: Ord a => Set a -> Set a -> Set a
intersection x y = if size x < size y then f x y else f y x
  where
    f smaller bigger =
      fromList [ x | x <- toList smaller, x `member` bigger ]

difference :: Ord a => Set a -> Set a -> Set a
difference x y = fromList [ e | e <- toList x, not (e `member` y) ] -- not tried yet

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
    -- assert (x > e)
    mkNode l e (insert x r)

-- rebalance during node construction
mkNode :: Ord a => Set a -> a -> Set a -> Set a
mkNode l e r = case (l,r) of { (Empty,Empty) -> singleton e ; _ -> do
  let u = height l - height r
  if u <=1 && u >= -1 then nodeSH l e r else
    if u < 0
    then
      case r of
        Empty -> undefined -- impossible: height(r) >= 2
        Node {left=rl,elem=re,right=rr} ->
          nodeSH (nodeSH l e rl) re rr
    else
      case l of
        Empty -> undefined  -- impossible: height(l) >= 2
        Node {left=ll,elem=le,right=lr} ->
          nodeSH ll le (nodeSH lr e r) }

nodeSH :: Ord a => Set a -> a -> Set a -> Set a
nodeSH left elem right =
  Node { left
       , elem
       , right
       , sizeNode = 1 + size left + size right
       , heightNode = 1 + max (height left) (height right)
       }

delete :: Ord a => a -> Set a -> Set a
delete x = \case
  Empty -> Empty
  Node {left=l,elem=e,right=r} ->
    if x < e then mkNode (delete x l) e r else
    if x > e then mkNode l e (delete x r) else
    -- assert (x==e)
    abut (l,r)

abut :: Ord a => (Set a,Set a) -> Set a
abut = \case
  (Empty,Empty) -> Empty
  (Empty,set) -> set
  (set,Empty) -> set
  (Node{left=l1,elem=e1,right=r1}, Node{left=l2,elem=e2,right=r2}) -> do
    -- assert (e1 < e2)
    mkNode l1 e1 (mkNode (abut (r1,l2)) e2 r2)

-- WIP...
-- prim def for union -- currently broken! -- and use for insert

_insertB :: Ord a => a -> Set a -> Set a
_insertB x set = singleton x `unionB` set

unionB :: Ord a => Set a -> Set a -> Set a
unionB s1 s2 =
  case s1 of
    Empty{} -> s2
    Node{left=l1,elem=e1,right=r1} ->
      case s2 of
        Empty -> s1
        Node{left=l2,elem=e2,right=r2} ->
          if e1 == e2
          then undefined
          else if e1 < e2
               -- broken because...
               then
                 -- some elements in r1 might be smaller than e1
                 -- and some elements in l2 might be bigger than e2
                 mkNode (mkNode l1 e1 (r1 `unionB` l2)) e2 r2

               else
                 mkNode (mkNode l2 e2 (r2 `unionB` l1)) e1 r1
