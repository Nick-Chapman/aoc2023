
module Misc
  ( check,collate,look,splitOn,the,hist,nub,pairwise
  , memoA, memoA2, memoA3
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Set
import Data.Array (Ix,array,range,(!))
import Data.Tuple.Extra (curry3,uncurry3)

check :: (Eq a, Show a) => a -> a -> a
check a b = if a == b then a else error ("check failed: " ++ show a ++ " not same as: " ++ show b)

collate :: Ord k => [(k,v)] -> [(k,[v])]
collate xs = Map.toList (Map.fromListWith (++) [ (k,[v]) | (k,v) <- xs ])

look :: (Ord k, Show k) => k -> Map k v -> v
look k m = maybe (error (show ("look",k))) id $ Map.lookup k m

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delimiter xs = loop [] xs
  where
    loop acc = \case
      [] -> [reverse acc]
      x:xs ->
        if x == delimiter
        then reverse acc : loop [] xs
        else loop (x:acc) xs

the :: [a] -> a
the = \case [x] -> x; xs -> error (show ("the",length xs))

hist :: (Ord a, Eq a) => [a] -> Map a Int
hist = Map.fromListWith (+) . map (\k -> (k,1))

nub :: Ord a => [a] -> [a]
nub = Set.toList . Set.fromList

pairwise :: [a] -> [(a,a)]
pairwise = \case
  [] -> []
  x1:xs -> [ (x1,x2) | x2 <- xs ] ++ pairwise xs


-- seem to loose cross module inlining by defining these here.
-- causes about 4x slowdown. very annoying

memoA :: Ix a => (a,a) -> (a -> b) -> (a -> b)
memoA r = (!) . \f -> array r [ (x,f x) | x <- range r ]

memoA2 :: (Ix a, Ix b) => ((a,b),(a,b)) -> (a -> b -> c) -> (a -> b -> c)
memoA2 r = curry . memoA r . uncurry

memoA3 :: (Ix a, Ix b, Ix c) => ((a,b,c),(a,b,c)) -> (a -> b -> c -> d) -> (a -> b -> c -> d)
memoA3 r = curry3 . memoA r . uncurry3
