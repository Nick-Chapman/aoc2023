module Day2 (main) where

import Misc (check)
import Par4 as Par (Par,parse,separated,nl,key,int,alts,ws1)

main :: IO ()
main = do
  sam1 <- readFile "../input/day2-sample.input"
  inp <- readFile "../input/day2.input"
  print ("day2, part1 (sam)", check [1,2,5] $ part1 (parse gram sam1))
  print ("day2, part1", check 2716 $ sum (part1 (parse gram inp)))
  print ("day2, part2 (sam)", check 2286 $ sum (part2 (parse gram sam1)))
  print ("day2, part2", check 72227 $ sum (part2 (parse gram inp)))

part1 :: [Game] -> [Int]
part1 gs =
   [ x
   | Game x ps <- gs
   , not $ any id [ toobig p | p <- ps ]
   ]
  where
    toobig :: RGB -> Bool
    toobig (r,g,b) = r>12 || g>13 || b>14

part2 :: [Game] -> [Int]
part2 gs =
   [ power rgb
   | Game _ ps <- gs
   , let rgb = foldr maxx zero ps
   ]
  where
    power :: RGB -> Int
    power (r,g,b) = r*g*b

    maxx :: RGB -> RGB -> RGB
    maxx (r1,g1,b1) (r2,g2,b2) = (max r1 r2, max g1 g2, max b1 b2)

data Game = Game Int [RGB] deriving Show
type RGB = (Int,Int,Int)

gram :: Par [Game]
gram = separated nl game
  where
    game :: Par Game
    game = do
      key "Game "
      id <- int
      key ": "
      xs <- picks
      pure (Game id xs)

    picks :: Par [RGB]
    picks = separated (key "; ") pick

    pick :: Par RGB
    pick = foldr add zero <$> separated (key ", ") pickC

    pickC :: Par RGB
    pickC = do
      n <- int
      ws1
      rgb n

    rgb :: Int -> Par RGB
    rgb x = alts
      [ do key "red" ; pure (x,0,0)
      , do key "green" ; pure (0,x,0)
      , do key "blue" ; pure (0,0,x)
      ]

add :: RGB -> RGB -> RGB
add (r1,g1,b1) (r2,g2,b2) = (r1+r2,g1+g2,b1+b2)

zero :: RGB
zero = (0,0,0)
