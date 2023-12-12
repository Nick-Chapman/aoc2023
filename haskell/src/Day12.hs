module Day12 (main) where

import Misc (check)
import Par4 (Par,parse,separated,nl,many,sp,lit,int,alts)
import Data.List (intercalate)

gram :: Par [Line]
gram = separated nl line
  where
    line = do
      xs <- many cell
      sp
      ns <- separated (lit ',') int
      pure (Line xs ns)
    cell = alts [ do lit '.'; pure Dot
                , do lit '#'; pure Hash
                , do lit '?'; pure Query ]

data Line = Line [Cell] [Int] deriving Show
data Cell = Dot | Hash | Query deriving Show

main :: IO ()
main = do
  let load x = parse gram <$> readFile x
  sam <- load "../input/day12-sample1.input"
  inp <- load "../input/day12.input"
  print ("day12, part1 (sam)", check [1,4,1,1,4,10] $ part1 sam)
  print ("day12, part1", check 7843 $ sum (part1 inp))
  print ("day12, part2 (sam)", check [1,16384,1,16,2500,506250] $ part2 sam)

  let res = part2 inp
  mapM_ print (zip [1::Int ..] res)
  print ("day12, part2", check 10153896718999 $ sum res)

part1 :: [Line] -> [Int]
part1 = map countFast

part2 :: [Line] -> [Int]
part2 = map (countFast . rep5)

rep5 :: Line -> Line
rep5 (Line xs ns) = Line xs' ns'
  where
    xs' = intercalate [Query] (take 5 (repeat xs))
    ns' = concat (take 5 (repeat ns))

countFast :: Line -> Int
countFast = countChunks . line2chunks

data Chunks = Chunks [[HQ]] [Int] deriving Show
data HQ = H | Q deriving (Eq,Show)

line2chunks :: Line -> Chunks
line2chunks (Line xs ns) = Chunks (chunkCells xs) ns

chunkCells :: [Cell] -> [[HQ]]
chunkCells = dot
  where
    dot :: [Cell] -> [[HQ]]
    dot = \case
      [] -> []
      Dot:xs -> dot xs
      Hash:xs -> hash [H] xs
      Query:xs -> hash [Q] xs
    hash :: [HQ] -> [Cell] -> [[HQ]]
    hash acc = \case
      [] -> [reverse acc]
      Dot:xs -> reverse acc : dot xs
      Hash:xs -> hash (H:acc) xs
      Query:xs -> hash (Q:acc) xs

splits :: [a] -> [([a],[a])]
splits = \case
  [] -> [([],[])]
  x:xs -> ([],x:xs) : [ (x:ys,zs) | (ys,zs) <- splits xs ]

countChunks :: Chunks -> Int
countChunks (Chunks xss ns) = do
  case ns of
    [] -> if any (==H) (concat xss) then 0 else 1::Int
    _:_ -> do
      case xss of
        [] -> undefined
        [hqs] -> countHQs hqs ns
        _:_ -> do
          let (xss1,xss2) = divide xss
          sum [ mul
                (countChunks (Chunks xss1 ns1))
                (countChunks (Chunks xss2 ns2))
              | (ns1,ns2) <- splits ns
              ]

mul :: Int -> Int -> Int
mul 0 _ = 0
mul a b = a * b

divide :: [a] -> ([a],[a])
divide = \case
  [] -> error "divide"
  xs -> do
    let n = length xs `div` 2
    (take n xs, drop n xs)

countHQs :: [HQ] -> [Int] -> Int
countHQs xs ns =
  case ns of
    [] -> if any (==H) xs then 0 else 1::Int
    _:_ -> do
      let (ns1,ns2) = divide ns
      case ns2 of
        [] -> error "impossible"
        nPick:ns2 -> do
          sum [ mul
                (countHQs xs1 ns1)
                (countHQs xs2 ns2)
              | (xs1,xs2) <- splitHashN0 nPick xs
              ]

splitHashN0 :: Int -> [HQ] -> [([HQ],[HQ])]
splitHashN0 nPick xs = do
  let more = splitHashN nPick xs
  case worksStart xs of
    Nothing -> more
    Just ys -> ([],ys) : more
  where
    worksStart :: [HQ] -> Maybe [HQ]
    worksStart xs =
      if length xs < nPick then Nothing else
        case drop nPick xs of
          [] -> Just []
          H:_ -> Nothing
          Q:xs -> Just xs

splitHashN :: Int -> [HQ] -> [([HQ],[HQ])]
splitHashN nPick = \case
  [] -> []
  x:xs -> do
    let more = [ (x:ys,zs) | (ys,zs) <- splitHashN nPick xs ]
    case worksHere (x:xs) of
      Nothing -> more
      Just ys -> ([],ys) : more
  where
    worksHere :: [HQ] -> Maybe [HQ]
    worksHere = \case
      [] -> Nothing
      H:_ -> Nothing
      Q:xs -> do
        if length xs < nPick then Nothing else
          case drop nPick xs of
            [] -> Just []
            H:_ -> Nothing
            Q:xs -> Just xs
