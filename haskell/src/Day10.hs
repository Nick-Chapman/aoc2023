module Day10 (main) where

import Data.Map (Map)
import Data.Set (Set)
import Misc (the,check)
import Par4 (Par,parse,separated,nl,many,dot)
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  let load x = parse gram <$> readFile x
  sam1 <- load "../input/day10-sample.input"
  sam2 <- load "../input/day10-sample2.input"
  inp <-  load "../input/day10.input"
  print ("day10, part1 (sam1)", check 4 $ part1 sam1)
  print ("day10, part1 (sam2)", check 8 $ part1 sam2)
  print ("day10, part1", check 7066 $ part1 inp)
  sam3 <- load "../input/day10-sample3.input"
  sam4 <- load "../input/day10-sample4.input"
  sam5 <- load "../input/day10-sample5.input"
  print ("day10, part2 (sam3)", check 4 $ part2 sam3)
  print ("day10, part2 (sam4)", check 8 $ part2 sam4)
  print ("day10, part2 (sam5)", check 10 $ part2 sam5)
  print ("day10, part2", check 401 $ part2 inp)


gram :: Par Grid
gram = separated nl (many dot)

type Grid = [[Char]]

data Dir = U | D | L | R deriving Show
type Pos = (Int,Int)


part1 :: Grid -> Int
part1 grid = do
  let
    start :: Pos
    start = the [ (x,y)
                | (y,line) <- zip [1..] grid
                , (x,'S') <- zip [1..] line ]
  let
    m :: Map Pos Char
    m = Map.fromList [ ((x,y),c)
                     | (y,line) <- zip [1..] grid
                     , (x,c) <- zip [1..] line ]
  let
    charAt :: Pos -> Char
    charAt pos = maybe '.' id (Map.lookup pos m)
  let
    loop :: Int -> Dir -> Pos -> Maybe Int
    loop n d pos =
      if pos == start then Just n else
        case step d (charAt pos) of
          Nothing -> Nothing
          Just d -> loop (n+1) d (adj pos d)

  head [ n | d <- [U,D,L,R] , Just n <- [loop 1 d (adj start d)] ] `div` 2


part2 :: Grid -> Int
part2 grid = do
  let
    start :: Pos
    start = the [ (x,y)
                | (y,line) <- zip [1..] grid
                , (x,'S') <- zip [1..] line ]
  let
    m :: Map Pos Char
    m = Map.fromList [ ((x,y),c)
                     | (y,line) <- zip [1..] grid
                     , (x,c) <- zip [1..] line ]
  let
    charAt :: Pos -> Char
    charAt pos = maybe '.' id (Map.lookup pos m)
  let
    loop :: Set Pos -> Dir -> Pos -> Maybe (Set Pos)
    loop acc d pos =
      if pos == start then Just acc else
        case step d (charAt pos) of
          Nothing -> Nothing
          Just d -> loop (Set.insert pos acc) d (adj pos d)
  let
    loopSetsByDirection =
      [ (set,d)
           | d <- [U,D,L,R]
           , Just set <- [ loop (Set.singleton start) d (adj start d) ]
           ]
  let startPipeChar = determineStartPipe (map snd loopSetsByDirection)
  let perimeter = fst (head loopSetsByDirection)
  let
    countCrossings :: Pos -> Int
    countCrossings (x,y) =
      length [ ()
             | n <- [1..min x y - 1] -- moving diagonally.
             , let pos = (x-n,y-n)
             , pos `Set.member` perimeter
             , crossingType (let c = charAt pos in
                             if c =='S' then startPipeChar else c)
             ]
  let
    isInside :: Pos -> Bool
    isInside pos =
      pos `Set.notMember` perimeter && odd (countCrossings pos)

  length (filter isInside (allPos grid))


allPos :: Grid -> [Pos]
allPos grid =
  [ (x,y)
  | y <- [1..length grid]
  , x <- [1..length (head grid)]
  ]

crossingType :: Char -> Bool
crossingType = (`elem` "-|FJ") -- not L/7

determineStartPipe :: [Dir] -> Char
determineStartPipe = \case
  [D,R] -> 'F'
  [D,L] -> '7'
  -- Didn't see these...
  --[U,R] -> 'L'
  --[U,L] -> 'J'
  --[U,D] -> '|'
  --[L,R] -> '-'
  xs -> error (show xs)

adj :: Pos -> Dir -> Pos
adj (x,y) = \case
  L -> (x-1,y)
  R -> (x+1,y)
  U -> (x,y-1)
  D -> (x,y+1)

step :: Dir -> Char -> Maybe Dir
step = \case
  U -> \case '|' -> Just U
             'F' -> Just R
             '7' -> Just L
             _ -> Nothing
  D -> \case '|' -> Just D
             'L' -> Just R
             'J' -> Just L
             _ -> Nothing
  L -> \case '-' -> Just L
             'L' -> Just U
             'F' -> Just D
             _ -> Nothing
  R -> \case '-' -> Just R
             'J' -> Just U
             '7' -> Just D
             _ -> Nothing
