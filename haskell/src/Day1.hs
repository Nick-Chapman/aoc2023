module Day1 (main) where

import Misc (check)
import Data.Char (ord)

main :: IO ()
main = do
  sam1 <- readFile "../input/day1-sample-part1.input"
  sam2 <- readFile "../input/day1-sample-part2.input"
  inp <- readFile "../input/day1.input"
  print ("day1, part1", check 142 $ part1 sam1)
  print ("day1, part1", check 55172 $ part1 inp)
  print ("day1, part2", check 281 $ part2 sam2)
  print ("day1, part2", check 54925 $ part2 inp) -- not 54953!

part1 :: String -> Int
part1 s = sum [ 10 * toD (head ds) + toD (head (reverse ds))
              | line <- lines s
              , let ds = [ c | c <- line, isD c ] ]

part2 :: String -> Int
part2 s =  sum [ 10 * toD (head ds1) + toD (head (reverse ds2))
               | line <- lines s
               , let ds1 = [ c | c <- convLtoR line, isD c ]
               , let ds2 = [ c | c <- convRtoL line, isD c ] ]

isD :: Char -> Bool
isD c = (ord c <= ord '9') && (ord '0' <= ord c)

toD :: Char -> Int
toD c = ord c - ord '0'

convRtoL :: String -> String
convRtoL = \case
  [] -> []
  x:xs -> conv (x : convRtoL xs)

convLtoR :: String -> String
convLtoR = \case
  [] -> []
  s -> case conv s of [] -> error "impossible"; x:xs -> x : convLtoR xs

conv :: String -> String
conv = \case
  'o':'n':'e'           :s -> '1':s
  't':'w':'o'           :s -> '2':s
  't':'h':'r':'e':'e'   :s -> '3':s
  'f':'o':'u':'r'       :s -> '4':s
  'f':'i':'v':'e'       :s -> '5':s
  's':'i':'x'           :s -> '6':s
  's':'e':'v':'e':'n'   :s -> '7':s
  'e':'i':'g':'h':'t'   :s -> '8':s
  'n':'i':'n':'e'       :s -> '9':s
  s                        ->     s
