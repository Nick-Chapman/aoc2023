
module Day22 (main) where

import Data.Array (Array,array,(!))
import Data.List (sortBy,nub)
import Data.Map (Map)
import Data.Ord (comparing)
import Data.Set (Set)
import GHC.Ix (Ix)
import Misc (check,collate)
import Par4 (Par,parse,separated,nl,lit,int)
import qualified Data.Char as Char (chr,ord)
import qualified Data.Map as Map
import qualified Data.Set as Set

gram :: Par Spec
gram = Spec <$> separated nl line
  where
    line :: Par Line
    line = do
      a <- pos3
      lit '~'
      b <- pos3
      pure (a,b)
    pos3 :: Par Pos3
    pos3 = do
      x <- int
      lit ','
      y <- int
      lit ','
      z <- int
      pure (x,y,z)

data Spec = Spec [Line]
type Line = (Pos3,Pos3)
type Pos3 = (Int,Int,Int)

main :: IO ()
main = do
  let load x = parse gram <$> readFile x
  _sam <- load "../input/day22-sample.input"
  _inp <- load "../input/day22.input"
  s <- part1 _sam
  print ("day22, part1 (sam)", check 5 $ s)
  i <- part1 _inp
  print ("day22, part1", check 457 $ i)
  s2 <- part2 _sam
  print ("day22, part2 (sam)", check 7 $ s2)
  i2 <- part2 _inp
  print ("day22, part2", check 110803 $ i2) -- WRONG -- too high

newtype Piece = Piece Int deriving (Eq,Ord,Ix)
instance Show Piece where
  show (Piece n) =
    if False
    then [Char.chr (Char.ord 'A' + n)]
    else "P" ++ show n

type Pos2 = (Int,Int)
data Classified = Classified { xy :: Pos2, z :: Int, dim :: Dim, len :: Int } deriving Show
data Dim = X | Y | Z deriving Show

classify :: Line -> Classified
classify ((x1,y1,z1),(x2,y2,z2)) = Classified { xy,z,dim,len }
  where
    xy = (min x1 x2, min y1 y2)
    z = min z1 z2
    lenX = abs (x1-x2)+1
    lenY = abs (y1-y2)+1
    lenZ = abs (z1-z2)+1
    dim = if x1 /= x2 then X else if y1 /= y2 then Y else Z
    len = case dim of X -> lenX; Y -> lenY; Z -> lenZ

part1 :: Spec -> IO Int
part1 spec@(Spec lines) = do
  let n = length $ nub [ b | (_,[b]) <- calcSupport spec ]
  pure (length lines - n)


part2 :: Spec -> IO Int
part2 spec = do
  let sups = calcSupport spec
  --mapM_ print sups
  let iv = collate [ (b,a) | (a,bs) <-sups, b <- bs ]
  --putStrLn ""
  --mapM_ print iv
  let m :: Map Piece [Piece] = Map.fromList iv
  let all = [ p | (p,_) <- sups ]
  let
    -- get wong answer fast!
    a :: Array Piece (Set Piece)
    a = array (minimum all, maximum all) [ (p, supported p) | p <- all ]

    supported :: Piece -> Set Piece
    supported x =
        case Map.lookup x m of
          Nothing -> Set.empty
          Just ys -> Set.unions [ Set.insert y $ a!y | y <- ys ]
  --let all = [ p | (p,_) <- sups ]
  --let xs = [ (p,supported p) | p <- all ]
  --putStrLn ""
  --mapM_ print xs
  let critical = nub [ b | (_,[b]) <- calcSupport spec ]
  let xs = [ Set.size (supported p) | p <- critical ]
  --mapM_ print (zip [1::Int ..] xs)
  let n = sum xs
  --print ("sum",n)
  pure n

type Sup = (Piece,[Piece]) -- (a,bs) : a rests on all of bs

calcSupport :: Spec -> [Sup]
calcSupport (Spec lines) = do
  let xs = [ (line, classify line,n) | (line,n) <- zip lines [0::Int .. ] ]
  let ys = sortBy (comparing byZ) xs
        where
          byZ :: (Line,Classified,Int) -> Int
          byZ (_,Classified{z},_) = z
  loop [] state0 ys
  where
    loop :: [Sup] -> State -> [(Line,Classified,Int)] -> [Sup]
    loop acc s = \case
      [] -> acc
      (_line,c,n):xs -> do
        let piece = Piece n
        --print ("PLACE",piece,_line,c)
        let (s',onPs0) = addLine c piece s
        let onPs = nub onPs0
        --print (piece,"on",onPs)
        --print s'
        let acc' = (piece,onPs) : acc
        loop acc' s' xs

type State = Map Pos2 (Int,Piece)
state0 :: State
state0 = Map.empty

addLine :: Classified -> Piece -> State -> (State,[Piece])
addLine Classified{xy=(x0,y0),dim,len} piece m = do
  let
    look :: Pos2 -> (Int,Maybe Piece)
    look p =
      case Map.lookup p m of
        Just (h,p) -> (h,Just p)
        Nothing -> (0,Nothing)
  let (d,xys) =
        case dim of
          X -> (1,  [(x,y0) | x <- [x0.. x0+len-1] ])
          Y -> (1,  [(x0,y) | y <- [y0.. y0+len-1] ])
          Z -> (len,[(x0,y0)])

  let hs :: [Int] = [ fst (look xy) | xy <- xys ]
  let h = maximum hs
  let h' = h + d
  let onPs :: [Piece] = [ p | xy <- xys, (n,Just p) <- [look xy], n==h ]
  (foldl (\m xy -> Map.insert xy (h',piece) m) m xys, onPs)
