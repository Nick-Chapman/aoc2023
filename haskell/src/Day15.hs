module Day15 (main) where

import Misc (check)
import Par4 (Par,parse,separated,many,sat,lit,word,alts,int)
import Data.Char (ord)
import Data.Map (Map)
import qualified Data.Map as Map

gram :: Par [String]
gram = separated (lit ',') (many notComma)
  where notComma = sat $ \c -> c /= ',' && c /= '\n'

gram2 :: Par [Step]
gram2 = separated (lit ',') step
  where
    step = do
      label <- word
      op <- alts [do lit '-'; pure Remove
                 ,do lit '='; Assign <$> int]
      pure (label,op)

type Step = (String,Op)
data Op = Remove | Assign Int deriving Show

main :: IO ()
main = do
  let load x = parse gram <$> readFile x
  sam <- load "../input/day15-sample.input"
  inp <- load "../input/day15.input"
  print ("day15, part1 (sam)", check [52] $ part1 ["HASH"])
  print ("day15, part1 (sam)", check 1320 $ sum (part1 sam))
  print ("day15, part1", check 521434 $ sum (part1 inp))

  let load2 x = parse gram2 <$> readFile x
  sam2 <- load2 "../input/day15-sample.input"
  inp2 <- load2 "../input/day15.input"
  print ("day15, part2 (sam)", check [1,4,28,40,72] $ part2 sam2)
  print ("day15, part2 (sam)", check 145 $ sum (part2 sam2))
  print ("day15, part2", check 248279 $ sum (part2 inp2))

hash :: String -> Int
hash = foldl f 0
  where f acc x = ((acc + ord x) * 17) `mod` 256

part1 :: [String] -> [Int]
part1 = map hash

part2 :: [Step] -> [Int]
part2 xs = power (foldl step state0 xs)

power :: State -> [Int]
power s =
  [ (1+bn) * sn * lens
  | (bn,box) <- Map.toList s
  , (sn,(_,lens)) <- zip [1::Int ..] box
  ]

type State = Map Int Box
type Box = [(String,Int)]

state0 :: State
state0 = Map.empty

step :: State -> Step -> State
step s (label,op) = do
  let h = hash label
  let b = maybe [] id $ Map.lookup h s
  case op of
    Remove -> Map.insert h (removeFromBox label b) s
    Assign n -> Map.insert h (placeInBox label n b) s

removeFromBox :: String -> Box -> Box
removeFromBox label = loop
  where
    loop = \case
      [] -> []
      x@(label',_):xs -> if label==label' then xs else x : loop xs

placeInBox :: String -> Int -> Box -> Box
placeInBox label n = loop
  where
    loop = \case
      [] -> [(label,n)]
      x@(label',_):xs -> if label==label' then (label,n):xs else x : loop xs
