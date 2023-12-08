module Day8 (main) where

import Data.Map (Map)
import Misc (check,look)
import Par4 (Par,parse,separated,nl,many,alts,lit,key,some,sat)
import qualified Data.Char as Char
import qualified Data.Map as Map

main :: IO ()
main = do
  let load x = parse gram <$> readFile x
  sam1 <- load "../input/day8-sample1.input"
  sam2 <- load "../input/day8-sample2.input"
  sam3 <- load "../input/day8-sample3.input"
  inp <-  load "../input/day8.input"
  print ("day8, part1 (sam1)", check 2 $ part1 sam1)
  print ("day8, part1 (sam2)", check 6 $ part1 sam2)
  print ("day8, part1", check 16343 $ part1 inp)
  print ("day8, part2 (sam3)", check 6 $ part2 sam3)
  print ("day8, part2", check 15299095336639 $ part2 inp)

data Setup = Setup [Dir] [Line] deriving Show
type Line = (Id,(Id,Id))
data Dir = L | R deriving Show
type Id = String

gram :: Par Setup
gram = do
  xs <- many dir
  nl; nl
  ys <- separated nl line
  pure (Setup xs ys)
  where
    dir :: Par Dir
    dir = alts [ do lit 'L'; pure L
               , do lit 'R'; pure R ]
    line :: Par Line
    line = do
      a <- word
      key " = ("
      b <- word
      key ", "
      c <- word
      lit ')'
      pure (a,(b,c))

    word =
      reverse <$>
      (some $ sat (\c -> Char.isAlpha c || Char.isDigit c))

part1 :: Setup -> Int
part1 (Setup path0 lines) = loop 0 path0 "AAA"
  where
    m :: Map Id (Id,Id)
    m = Map.fromList lines

    loop :: Int -> [Dir] -> Id -> Int
    loop n path x =
      if x == "ZZZ" then n else
        case path of
          [] -> loop n path0 x
          d:path -> loop (1+n) path (step x d)

    step :: Id -> Dir -> Id
    step x d = pick (look x m) d

    pick (b,c) = \case L -> b; R -> c


part2 :: Setup -> Int
part2 (Setup path0 lines) = do
  let start = [ x | (x@(c:_),_) <- lines, c == 'A' ]
  let res = [ fst (loop 0 path0 x) | x <- start ]
  foldl lcm 1 res

  where
    finished :: Id ->  Bool
    finished = \case 'Z':_ -> True; _ -> False

    m :: Map Id (Id,Id)
    m = Map.fromList lines

    loop :: Int -> [Dir] -> Id -> (Int,Id)
    loop n path x =
      if finished x && n > 0 then (n,x) else
        case path of
          [] -> loop n path0 x
          d:path -> loop (1+n) path (step d x)

    step :: Dir -> Id -> Id
    step d x = pick (look x m) d

    pick (b,c) = \case L -> b; R -> c
