module Advent2023 where

import Control.Monad (forM_)
import Data.List (sortBy)
import Data.Ord (comparing)
import GHC.Int (Int64)
import System.Clock (TimeSpec(..),getTime,Clock(Monotonic))
import System.Environment (getArgs)
import Text.Printf (printf)

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11

mains :: [(Int,IO ())]
mains = zip [1..]
  [ Day1.main
  , Day2.main
  , Day3.main
  , Day4.main
  , Day5.main
  , Day6.main
  , Day7.main
  , Day8.main
  , Day9.main
  , Day10.main
  , Day11.main
  ]

main :: IO ()
main = do
  args <- getArgs
  let selected = if args == [] then map fst mains else map read args
  let picked = [ x | x@(i,_) <- mains, i `elem` selected ]
  _info <- sequence [ timed day io | (day,io) <- picked ]
  if length picked > 1 then printTimings _info else pure ()

data Timing = Timing { day :: Int, time :: Nanos }

timed :: Int -> IO () -> IO Timing
timed day io = do
  before <- getTime Monotonic
  io
  after <- getTime Monotonic
  let TimeSpec{sec,nsec} = after - before
  let time = Nanos (gig * sec + nsec)
  pure $ Timing {day,time}

printTimings :: [Timing] -> IO ()
printTimings xs = do
  putStrLn "\ntimings:"
  forM_ (sortBy (comparing time) xs) $ \Timing{day,time} -> do
    putStrLn (printf "- day %2d : " day ++ show time)
  putStrLn $ "\ntotal = " ++ show (sum (map time xs))

newtype Nanos = Nanos Int64 deriving (Eq,Ord,Num)

instance Show Nanos where
  show (Nanos i) = printf "%.03fs" dub
    where dub :: Double = fromIntegral i / fromIntegral gig

gig :: Int64
gig = 1_000_000_000
