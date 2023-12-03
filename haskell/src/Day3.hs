module Day3 (main) where

import Misc (check,collate)
import Data.Char (isDigit)

main :: IO ()
main = do
  sam <- readFile "../input/day3-sample.input"
  inp <- readFile "../input/day3.input"
  print ("day1, part1 (sam)", check 4361 $ sum (part1 sam))
  print ("day1, part1", check 553079 $ sum (part1 inp))
  print ("day1, part2 (sam)", check 467835 $ sum (part2 sam))
  print ("day1, part2", check 84363105 $ sum (part2 inp))

type Pos = (Int,Int)
data Entry = Entry { pos :: Pos, len :: Int, num :: Int } deriving Show

part1 :: String -> [Int]
part1 s = do
  let symbolPositions :: [Pos] = concat
        [ [ (x,y) | (x,c) <- zip [0..] line, not (isDigit c || c == '.') ]
        | (y,line) <- zip [0..] (lines s)
        ]
  [ num | e@Entry{num} <- entries s
        , let ps = filter (`elem` symbolPositions) (adj e)
        , ps /= [] ]

part2 :: String -> [Int]
part2 s = do
  let starPositions :: [Pos] = concat
        [ [ (x,y) | (x,c) <- zip [0..] line, c == '*' ]
        | (y,line) <- zip [0..] (lines s)
        ]
  [ (n1*n2) | (_,[n1,n2]) <- collate [ (p,num)
                                     | e@Entry{num} <- entries s
                                     , let ps = filter (`elem` starPositions) (adj e)
                                     , p <- ps ] ]

entries :: String -> [Entry]
entries s0 =
  concat [ loopLine y 0 line | (y,line) <- zip [0..] (lines s0) ]
  where
    loopLine :: Int -> Int -> String -> [Entry]
    loopLine y x0 s0 = do
      case span (not . isDigit) s0 of
        (_,[]) -> []
        (s1,s2) -> do
          let x = length s1 + x0
          let (s3,s4) = span isDigit s2
          let num = read s3
          let len = length s3
          Entry { pos = (x,y), len, num } : loopLine y (x + len) s4

adj :: Entry -> [Pos]
adj Entry{pos=(x0,y),len} = do
  let (xA,xB) = (x0-1,x0+len)
  [(xA,y),(xB,y)] ++ concat [ [(x,y-1), (x,y+1)] | x <- [xA..xB] ]
