
module Day23 (main) where

import Data.Maybe (maybeToList) --,catMaybes)
import Data.Set (Set)
import Data.Map (Map)
import Misc (check)
import Par4 (Par,parse,separated,nl,lit,many,alts)
import qualified Data.Map as Map
import qualified Data.Set as Set

gram :: Par Maze
gram = separated nl (many cell)
  where
    cell :: Par Cell
    cell = alts [ do lit '.'; pure Dot
                , do lit '#'; pure Hash
                , do lit '<'; pure Langle
                , do lit '>'; pure Rangle
                , do lit '^'; pure Hat
                , do lit 'v'; pure Vee ]

type Maze = [[Cell]]
data Cell = Dot | Hash | Langle | Rangle | Hat | Vee deriving Show

main :: IO ()
main = do
  let load x = parse gram <$> readFile x
  _sam <- load "../input/day23-sample.input"
  _inp <- load "../input/day23.input"
  print ("day23, part1 (sam)", check 94 $ part1 _sam)
  print ("day23, part1", check 2018 $ part1 _inp)

  s2 <- part2 _sam
  print ("day23, part2 (sam)", check 154 $ s2)

  i2 <- part2 _inp
  print ("day23, part2", check 0 $ i2)

type Pos = (Int,Int)
data Dir = L | R | U | D deriving (Eq,Ord,Show)

type M = Map (Set Pos,(Dir,Pos)) (Maybe Int)

part2 :: Maze -> IO Int
part2 maze = do
  let h = length maze
  let w = length (head maze)
  let stop p = p == (w-1,h)
  let
    notSlippy :: Cell -> Cell
    notSlippy = \case Hash -> Hash; _ -> Dot
  let
    at :: Pos -> Maybe Cell
    at = flip Map.lookup m
      where m = Map.fromList
              [ ((x,y), notSlippy cell)
              | (y,line) <- zip [1..] maze , (x,cell) <- zip [1..] line
              ]
  let
    walk :: M -> Set Pos -> Int -> (Dir,Pos) -> IO (M, Maybe Int)
    walk m vs q dp = do
      --print (Map.size m, "walk",q,dp)
      case Map.lookup (vs,dp) m of
        Just res -> do
          print (Map.size m, "walk",q,dp)
          print "HIT"
          pure (m,res)
        Nothing -> do
          (m',res) <- walkF m vs q dp
          let m'' = Map.insert (vs,dp) res m'
          pure (m'',res)

    walkF :: M -> Set Pos -> Int -> (Dir,Pos) -> IO (M, Maybe Int)
    walkF m vs q dp = do
      --print ("walk",q,dp)
      --print ("walk",q,dp,"#",Set.toList vs)
      case walkToJunction stop at dp of
        Nothing -> do
          error "never here why?"
          --print ("walk",q,dp,"-->","DEAD-END/LOOP")
          --pure Nothing
        Just (i,opt) -> do
          case opt of
            Nothing -> do
              --print ("walk",q,dp,"-->","FINISHED")
              pure (m, Just i)
            Just (jpos,dps) -> do
              if jpos `Set.member` vs then pure (m,Nothing) else do
                let vs' = Set.insert jpos vs
                --print ("walk",q,dp,"-->",dps)
                let q' = q+1
                let
                  walks :: M -> [Int] -> [(Dir,Pos)] -> IO (M,[Int])
                  walks m acc = \case
                    [] -> pure (m,acc)
                    x:xs -> do
                      (m',opt) <- walk m vs' q' x
                      let acc' = (case opt of Nothing -> acc; Just n -> n:acc)
                      walks m' acc' xs

                --ns' <- mapM (walk vs' (q+1)) dps
                --let ns :: [Int] = catMaybes ns'
                (m,ns) <- walks m [] dps
                case ns of
                  [] -> pure (m,Nothing)
                  _ -> pure (m,Just (i + maximum ns))

  let start :: Pos = (2,1)
  let m0 = Map.empty
  walk m0 (Set.singleton (2,0)) 0 (D,start) >>= \case
    (_,Nothing) -> error "no solution"
    (_,Just res) -> pure res




part1 :: Maze -> Int
part1 maze = do
  let h = length maze
  let w = length (head maze)
  let stop p = p == (w-1,h)
  let
    at :: Pos -> Maybe Cell
    at = flip Map.lookup m
      where m = Map.fromList
              [ ((x,y),cell) | (y,line) <- zip [1..] maze , (x,cell) <- zip [1..] line ]
  let
    walk :: (Dir,Pos) -> Int
    walk dp = do
      case walkToJunction stop at dp of
        Nothing -> undefined
        Just (i,opt) -> do
          case opt of
            Nothing -> i
            Just (_,dps) -> i + maximum (map walk dps)
  let start :: Pos = (2,1)
  walk (D,start)

walkToJunction :: (Pos -> Bool) -> (Pos -> Maybe Cell) -> (Dir,Pos) -> Maybe (Int,Maybe (Pos,[(Dir,Pos)]))
walkToJunction stop at = loop 0
  where
    loop i (dirLast,pos) = if stop pos then Just (i,Nothing) else do
      let
        alts :: [(Dir,Pos)]
        alts =
          [ (dir,pos') | dir <- allWays
                       , dir /= opp dirLast
                       , let pos' = adj pos dir
                       , cell <- maybeToList (at pos')
                       , canStep cell dir
                       ]
      case alts of
        [] -> Nothing --error (show ("no alts",dirLast,pos))
        [dp] -> loop (i+1) dp
        _ -> Just (i+1,Just (pos,alts))

allWays :: [Dir]
allWays = [L,R,U,D]

adj :: Pos -> Dir -> Pos
adj (x,y) = \case L -> (x-1,y); R -> (x+1,y); U -> (x,y-1); D -> (x,y+1)

opp :: Dir -> Dir
opp = \case L -> R; R -> L; U -> D; D -> U

canStep :: Cell -> Dir -> Bool
canStep = \case
  Dot -> \_ -> True
  Hash -> \_ -> False
  Langle -> \case L -> True; _ -> False
  Rangle -> \case R -> True; _ -> False
  Hat -> \case U -> True; _ -> False
  Vee -> \case D -> True; _ -> False
