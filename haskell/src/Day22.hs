
module Day22 (main) where

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
  sam <- load "../input/day22-sample.input"
  inp <- load "../input/day22.input"
  s1 <- part1 sam
  print ("day22, part1 (sam)", check 5 $ s1)
  i1 <- part1 inp
  print ("day22, part1", check 457 $ i1)
  s2 <- part2 sam
  print ("day22, part2 (sam)", check 7 $ s2)
  i2 <- part2 inp
  print ("day22, part2", check 79122 $ i2)

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


part2 :: Spec -> IO Int
part2 spec = do
  let sup = calcSupport spec
  let all = map fst sup
  res <- sequence [ countFalls piece sup | piece <- all ]
  pure (sum res)

type SupM = Map Piece (Set Piece)

countFalls :: Piece -> [Sup] -> IO Int
countFalls piece initialSups = loop 0 Set.empty [piece] sup0
  where
    sup0 :: SupM -- supported by
    sup0 = Map.fromList [ (a,Set.fromList bs) | (a,bs) <- initialSups ]

    supports :: Piece -> [Piece]
    supports p = maybe [] id $ Map.lookup p inv
      where
        inv = Map.fromList $ collate [ (b,a)
                                     | (a,bs) <- initialSups, b <- bs ]

    loop :: Int -> Set Piece -> [Piece] -> SupM -> IO Int
    loop i acc frontier sup = do
      case frontier of
        [] -> pure (Set.size acc - 1)
        f1:frontier -> inner frontier sup (supports f1)
          where
            inner :: [Piece] -> SupM -> [Piece] -> IO Int
            inner frontier sup = \case
              [] -> loop (i+1) (Set.insert f1 acc) frontier sup
              x:xs -> do
                let ys = maybe (error "sup") id (Map.lookup x sup)
                let ys' = Set.delete f1 ys
                let sup' = Map.insert x ys' sup
                if Set.null ys'
                  then inner (x:frontier) sup' xs
                  else inner frontier sup' xs
