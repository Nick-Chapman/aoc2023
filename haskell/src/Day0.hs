module Day0 (main) where -- prepare setup using last year's day2

import Misc (check,the)
import Par4 (Par,parse,terminated,alts,nl,lit)

main :: IO ()
main = do
  inp <- parse gram <$> readFile "../input/day0.input"
  print ("day0, part1", check 11475 $ part1 inp)
  print ("day0, part2", check 16862 $ part2 inp)

gram :: Par Setup
gram = terminated nl line
  where
    i c v = do lit c; pure v
    line = do
      them <- alts [ i 'A' A, i 'B' B, i 'C' C ]
      lit ' '
      me <- alts [ i 'X' X, i 'Y' Y, i 'Z' Z ]
      pure (them,me)

type Setup = [(Them,Me)]

data Them    = A | B | C
data Me      = X | Y | Z
data Go      = Rock | Paper | Scissors
data Outcome = Win | Draw | Loss deriving Eq

goThem :: Them -> Go
goThem = \case A -> Rock; B -> Paper; C -> Scissors

scoreGo :: Go -> Int
scoreGo = \case Rock -> 1; Paper -> 2; Scissors -> 3

scoreOutcome :: Outcome -> Int
scoreOutcome = \case Win -> 6; Draw -> 3; Loss -> 0

part1 :: Setup -> Int
part1 xs =
  sum [ scoreOutcome (outcomeForPlayer2 p1 p2) + scoreGo p2
      | (them,me) <- xs
      , let p1 = goThem them
      , let p2 = goMe me
      ]
  where
    goMe :: Me -> Go
    goMe = \case X -> Rock; Y -> Paper; Z -> Scissors

part2 :: Setup -> Int
part2 xs =
  sum [ scoreOutcome outcome + scoreGo p2
      | (them,me) <- xs
      , let p1 = goThem them
      , let outcome = outcomeMe me
      , let p2 = makeOutcome p1 outcome
      ]
  where
    outcomeMe :: Me -> Outcome
    outcomeMe = \case X -> Loss; Y -> Draw; Z -> Win

outcomeForPlayer2 :: Go -> Go -> Outcome
outcomeForPlayer2 = x
  where
    x Rock     Rock     = Draw
    x Rock     Paper    = Win
    x Rock     Scissors = Loss
    x Paper    Rock     = Loss
    x Paper    Paper    = Draw
    x Paper    Scissors = Win
    x Scissors Rock     = Win
    x Scissors Paper    = Loss
    x Scissors Scissors = Draw

makeOutcome :: Go -> Outcome -> Go
makeOutcome p1 outcome =
  the [ p2
      | p2 <- [Rock,Paper,Scissors]
      , outcomeForPlayer2 p1 p2 == outcome
      ]
