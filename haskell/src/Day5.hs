module Day5 (main) where

import Misc (check)
import Par4 (Par,parse,separated,nl,key,int,terminated,sp,many,dot)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "../input/day5-sample.input"
  inp <- parse gram <$> readFile "../input/day5.input"
  print ("day5, part1 (sam)", check 35 $ part1 sam)
  print ("day5, part1", check 313045984 $ part1 inp)
  print ("day5, part2 (sam)", check 46 $ part2 sam)
  print ("day5, part2", check 20283860 $ part2 inp)

part1 :: Setup -> Int
--part1 (Setup xs ms) = minimum [ foldl appM1 x ms | x <- xs ]
part1 (Setup xs ms) = part2 (Setup ys ms) where ys = concat [ [x,1] | x <- xs ]

{-appM1 :: Int -> M -> Int
appM1 x (M trips) = loop trips
  where
    loop = \case
      [] -> x
      (d,s,l):trips -> do
        let n = x-s
        if n>=0 && n < l then d+n else loop trips-}

part2 :: Setup -> Int
part2 (Setup xs ms) = do
  minimum [ x | R x _ <- foldl appMR (makeRanges xs) ms ]
  where
    makeRanges = \case
      [] -> []
      [_] -> error "not even"
      x:n:xs -> R x (x+n-1) : makeRanges xs

data R = R Int Int
instance Show R where show (R x y) = show x ++ ".." ++ show y

appMR :: [R] -> M -> [R]
appMR rs (M trips) = concat [ loop r trips | r <- rs ]
  where
    loop r trips =
      if isEmpty r then [] else
      case trips of
        [] -> [r]
        (d,s,l):trips -> do
          let rline = R s (s+l-1)
          let (rsect,rout) = cutR r rline
          if isEmpty rsect
            then concat [ loop r trips | r <- rout ]
            else shiftR rsect (d-s) : concat [ loop r trips | r <- rout ]

shiftR :: R -> Int -> R
shiftR (R x y) n = R (x+n) (y+n)

cutR :: R -> R -> (R,[R])
cutR (R x1 y1) (R x2 y2) =

  (R (max x1 x2) (min y1 y2),
   [
     R (max x1 (y2 + 1)) y1,
     R x1 (min (x2 - 1) y1)
   ])

isEmpty :: R -> Bool
isEmpty (R x y) = y < x



data Setup = Setup [Int] [M] deriving Show
data M = M [(Int,Int,Int)] deriving Show

gram :: Par Setup
gram = setup
  where
    setup = do
      key "seeds: "
      xs <- separated sp int
      nl; nl
      ms <- separated nl map
      pure (Setup xs ms)

    map = do
      _ <- many dot
      nl
      M <$> terminated nl line

    line = do
      a <- int
      sp
      b <- int
      sp
      c <- int
      pure (a,b,c)
