module Day13 (main) where

import Data.List (nub,(\\),transpose)
import Misc (check,the)
import Par4 (Par,parse,separated,terminated,nl,some,dot)

type Pattern = [String]

gram :: Par [Pattern]
gram = separated nl pat where pat = terminated nl (some dot)

main :: IO ()
main = do
  let load x = parse gram <$> readFile x
  sam <- load "../input/day13-sample.input"
  inp <- load "../input/day13.input"
  print ("day13, part1 (sam)", check [5,400] $ part1 sam)
  print ("day13, part1", check 33195 $ sum (part1 inp))
  print ("day13, part2 (sam)", check [300,100] $ part2 sam)
  print ("day13, part2", check 31836 $ sum (part2 inp))

reflections :: Int -> Pattern -> [Int]
reflections m pat =
  [ m * length l | (l,r) <- candidateReflections pat, all id (zipWith (==) l r) ]

candidateReflections :: [a] -> [([a],[a])]
candidateReflections = \case [] -> []; x:xs -> loop [x] xs
  where loop xs = \case [] -> []; y:ys -> (xs, y:ys) : loop (y:xs) ys

part1 :: [Pattern] -> [Int]
part1 = map $ \p -> the (reflections 100 p ++ reflections 1 (transpose p))

part2 :: [Pattern] -> [Int]
part2 = map $ \p -> do
  let new = concat [ reflections 100 s ++ reflections 1 (transpose s) | s <- smudges p ]
  let old = reflections 100 p ++ reflections 1 (transpose p)
  the (nub new \\ old)

smudges :: Pattern -> [Pattern]
smudges = smudge (smudge (\c -> [flipPixel c]))

smudge :: (a -> [a]) -> [a] -> [[a]]
smudge f = \case
  [] -> error "smudges"
  [x] ->  [ [y] | y <- f x ]
  x:xs -> map (:xs) (f x) ++ map (x:) (smudge f xs)

flipPixel :: Char -> Char
flipPixel = \case '.' -> '#'; '#' -> '.'; _ -> error "flipPixel"
