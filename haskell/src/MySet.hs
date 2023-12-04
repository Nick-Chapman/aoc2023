
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
--difference = undefined
--difference x y = fromList [ e | e <- toList x, not (e `member` y) ]
difference x y = foldr delete x (toList y) -- ???

data Set a
  = Empty
  | Single a -- not required; just optimized rep
  | Node { left :: Set a
         , elem :: a
         , right :: Set a
         , sizeNode :: Int
         , heightNode :: Int
         }
    deriving (Eq,Ord) -- ok?

null :: Set a -> Bool
null = \case
  Empty{} -> True
  Single{} -> False
  Node{} -> False

empty :: Set a
empty = Empty

singleton :: a -> Set a
singleton = Single

toList :: Set a -> [a]
toList = walk []
  where
    walk :: [a] -> Set a -> [a]
    walk after = \case
      Empty -> after
      Single e -> e : after
      Node{left=l,elem=e,right=r} ->
        walk (e : walk after r) l

size :: Set a -> Int
size = \case
  Empty -> 0
  Single{} -> 1
  Node{sizeNode=x} -> x

height :: Set a -> Int
height = \case
  Empty -> 0
  Single{} -> 1
  Node{heightNode=x} -> x

notMember :: Ord a => a -> Set a -> Bool
notMember x = not . member x

member :: Ord a => a -> Set a -> Bool
member x = \case
  Empty -> False
  Single e -> x==e
  Node{left=l,elem=e,right=r} ->
    x == e || member x (if x < e then l else r)

insert :: Ord a => a -> Set a -> Set a
insert x set = if x `member` set then set else insert' x set -- helps?

insert' :: Ord a => a -> Set a -> Set a
insert' x = \case
  Empty -> singleton x
  n@(Single e) -> if x == e then n else if x < e then twoSet x e else twoSet e x
  n@Node{left=l,elem=e,right=r} -> if x == e then n else
    if x < e then mkNode (insert x l) e r else
    -- assert (x > e)
    mkNode l e (insert x r)

twoSet :: Ord a => a -> a -> Set a
twoSet x y = -- x<y
  nodeSH empty x (singleton y)

-- rebalance during node construction
mkNode :: Ord a => Set a -> a -> Set a -> Set a
mkNode l e r = case (l,r) of { (Empty,Empty) -> Single e ; _ -> do
  let u = height l - height r
  if u <=1 && u >= -1 then nodeSH l e r else
    if u < 0
    then
      case r of
        Empty -> undefined -- impossible: height(r) >= 2
        Single{} -> undefined -- impossible: height(r) >= 2
        Node {left=rl,elem=re,right=rr} -> nodeSH (nodeSH l e rl) re rr
    else
      case l of
        Empty -> undefined  -- impossible: height(l) >= 2
        Single{} -> undefined -- impossible: height(l) >= 2
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
  n@(Single e) -> if x==e then empty else n
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
  (Single e1,Single e2) -> twoSet e1 e2
  (Single e1,n2@Node{}) -> mkNode empty e1 n2
  (n1@Node{},Single e2) -> mkNode n1 e2 empty

  (Node{left=l1,elem=e1,right=r1}, Node{left=l2,elem=e2,right=r2}) -> do
    -- assert (e1 < e2)
    mkNode l1 e1 (mkNode (abut (r1,l2)) e2 r2)
