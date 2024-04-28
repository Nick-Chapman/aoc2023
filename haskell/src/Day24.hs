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
  print ("day24, part2 (sam)", check 47 $ part2 sam)
  print ("day24, part2", check 716599937560103 $ part2 inp)
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


part2 :: Spec -> Int
part2 spec = do
  -- pick 3 hailstrones
  let h0 = spec !! 0
  let h1 = spec !! 1
  let h2 = spec !! 2
  -- change frame-of-reference to stationery h0
  let r1 = h1 `rebase` h0
  let r2 = h2 `rebase` h0
  -- compute plane of h1 movement w.r.t h0
  let p1 = r1 `at` 0
  let q1 = r1 `at` 1
  let c1 = reduceP3 (p1 `cross` q1)
  -- compute plane of h2 movement w.r.t h0
  let p2 = r2 `at` 0
  let q2 = r2 `at` 1
  let c2 = reduceP3 (p2 `cross` q2)
  -- compute intersection of planes for thrown stone's velocoty vector
  let e0@(_,_,z0) = reduceP3 (c1 `cross` c2)
  let stoneVelVec = (if z0>0 then scale e0 (-1) else e0)
  -- construct two sets of 3 equations in four vars: x,y,z and t1/t2 for hailstones h1/h2
  let fourVarsT1 = makeEqn r1 stoneVelVec
  let fourVarsT2 = makeEqn r2 stoneVelVec
  -- solve for t1/t2, leaving equation in x,y,z
  let threeVars =
        [ reduceEqn (resolve e1 e2) | (e1:es) <- tails fourVarsT1, e2 <- es ] ++
        [ reduceEqn (resolve e1 e2) | (e1:es) <- tails fourVarsT2, e2 <- es ]
  -- solve for x, leaving equation in y,z
  let twoVars = [ reduceEqn (resolve e1 e2) | (e1:es) <- tails threeVars, e2 <- es ]
  -- solve for y, leaving equation in z
  let oneVars = [ reduceEqn r
                | (e1:es) <- tails twoVars, e2 <- es
                , r@([z],_) <- [resolve e1 e2] , z /= 0
                ]
  -- pick head equation from 3/2/1 vars sets
  let (x1,y1,z1,k1) = case threeVars of ([a,b,c],d):_ -> (a,b,c,d); _ -> undefined
  let (y2,z2,k2) = case twoVars of ([a,b],c):_ -> (a,b,c); _ -> undefined
  let (z3,k3) = case oneVars of ([a],b):_ -> (a,b); _ -> undefined
  -- solve initial position for z, then substitute back for y and x
  let zsol = k3 `pdiv` z3
  let ysol = (k2 - (z2 * zsol)) `pdiv` y2
  let xsol = (k1 - (y1 * ysol + z1 * zsol)) `pdiv` x1
  -- convert initial position back to original frame-of-reference
  let (p0,_) = h0
  let (x,y,z) = (xsol,ysol,zsol) `add` p0
  -- and add elements for final answer
  (x+y+z)

tails :: [a] -> [[a]]
tails = \case
  [] -> []
  xs@(_:ys) -> xs : tails ys

resolve :: Eqn -> Eqn -> Eqn
resolve (xs,xk) (ys,yk) =
  case (xs,ys) of
    ([],_) -> undefined
    (_,[]) -> undefined
    (x1:xs,y1:ys) -> do
      let comb x y = x1*y - y1*x
      ([ comb x y | (x,y) <- zip xs ys ], comb xk yk)

type Eqn = ([Int],Int)

makeEqn :: Line3 -> P3 -> [Eqn]
makeEqn ((x1,y1,z1),(vx1,vy1,vz1)) (vx,vy,vz) =
  [ ([-vx-vx1, 1, 0, 0], x1)
  , ([-vy-vy1, 0, 1, 0], y1)
  , ([-vz-vz1, 0, 0, 1], z1)
  ]

reduceEqn :: Eqn -> Eqn
reduceEqn (xs,x) = do
  let g = gcds (x:xs)
  ([ x `pdiv` g | x <- xs ], x `pdiv` g)

reduceP3 :: P3 -> P3
reduceP3 (x,y,z) = do
  let g = gcds [x,y,z]
  (x `pdiv` g, y `pdiv` g, z `pdiv` g)

pdiv :: Int -> Int -> Int
pdiv a b = do
  let g = gcd a b
  if (g /= b && g /= -b) then error (show ("pdiv",a,b,g)) else a `div` b

gcds :: [Int] -> Int
gcds = \case
  [] -> error "gcds"
  x:xs -> foldl gcd x xs

rebase :: Line3 -> Line3 -> Line3
rebase (p1,v1) (p2,v2) = (p1 `diff` p2, v1 `diff` v2)

at :: Line3 -> Int -> P3
at (p,v) time = p `add` scale v time

diff :: P3 -> P3 -> P3
diff p q = p `add` scale q (-1)

add :: P3 -> P3 -> P3
add (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)

scale :: P3 -> Int -> P3
scale (x,y,z) n = (x*n,y*n,z*n)

cross :: P3 -> P3 -> P3
cross (x1,y1,z1) (x2,y2,z2) = (y1*z2-z1*y2, z1*x2-x1*z2, x1*y2-y1*x2)
