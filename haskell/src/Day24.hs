module Day24 (main) where

import GHC.Float (int2Double)
import Misc (check)
import Par4 (Par,parse,terminated,nl,lit,int,ws0,alts)

gram :: Par Spec
gram = terminated nl line
  where
    line = do pos <- p3; ws0; lit '@'; ws0; vel <- p3; pure (pos,vel)
    p3 = do x <- num; com; y <- num; com; z <- num; pure (x,y,z)
    com = do lit ','; ws0
    num = alts [int, do lit '-'; (0-) <$> int]

main :: IO ()
main = do
  let load x = parse gram <$> readFile x
  sam <- load "../input/day24-sample.input"
  inp <- load "../input/day24.input"
  sam1 <- part1 (7,27) sam
  print ("day24, part1 (sam)", check 2 $ sam1)
  res1 <- part1 (200000000000000,400000000000000) inp
  print ("day24, part1", check 11246 $ res1)
  pure ()

type Spec = [Line3]
type Line3 = (P3,P3)
type P3 = (Int,Int,Int)
type P2 = (Int,Int)

part1 :: (Int,Int) -> Spec -> IO Int
part1 range lines = do
  results <- sequence
    [ do r <- compute1 range g h; pure (g,h,r)
    | (g',h') <- pairwise lines
    , let g = ignoreZline g'
    , let h = ignoreZline h'
    ]
  --mapM_ print results
  pure $ length [ () |  (_,_,Inside{}) <- results ]


ignoreZline :: (P3,P3) -> (P2,P2)
ignoreZline (pos,vel) = (ignoreZp pos, ignoreZp vel)

ignoreZp :: P3 -> P2
ignoreZp (x,y,_z) = (x,y)

pairwise :: [a] -> [(a,a)]
pairwise = \case
  [] -> []
  [_] -> []
  x:xs -> [(x,y) | y <- xs] ++ pairwise xs

data Res
  = NeverIntersect
  | PastA
  | PastB
  | PastBoth
  | Inside (Double,Double)
  | Outside (Double,Double)
  deriving Show

compute1 :: (Int,Int) -> (P2,P2) -> (P2,P2) -> IO Res
compute1 (rLO,rHI) g h = do
  --print g
  --print h
  let ((gix,giy),(gvx,gvy)) = g
  let ((hix,hiy),(hvx,hvy)) = h
  let a = gvx
  let b = -hvx
  let c = gvy
  let d = -hvy
  --print (a,b,c,d)
  let e = hix - gix
  let f = hiy - giy
  --print (e,f)
  let det = a*d - b*c
  --print det
  if det == 0 then pure NeverIntersect else do
    let det_tG = d*e + (-b)*f
    let det_tH = (-c)*e + a*f
    --print (det_tG,det_tH)
    let gFuture = sign det_tG == sign det
    let hFuture = sign det_tH == sign det
    case (gFuture,hFuture) of
      (False,False) -> pure PastBoth
      (False,True) -> pure PastA
      (True,False) -> pure PastB
      (True,True) -> do
        let tG = int2Double det_tG / int2Double det
        --let tH = int2Double det_tH / int2Double det
        --print (tG,tH)
        let x = int2Double gix + tG * int2Double gvx
        let y = int2Double giy + tG * int2Double gvy
        --print (x,y)
        let inRange n = n >= int2Double rLO && n <= int2Double rHI
        let inArea = inRange x && inRange y
        if inArea then pure $ Inside (x,y) else pure $ Outside (x,y)

sign :: Int -> Bool
sign x = x>=0
