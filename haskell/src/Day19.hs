
module Day19 (main) where

import Data.Map (Map)
import Misc (check,look)
import Par4 (Par,parse,separated,nl,int,alts,lit,terminated,word,noError)
import qualified Data.Map as Map

main :: IO ()
main = do
  let load x = parse gram <$> readFile x
  sam <- load "../input/day19-sample.input"
  inp <- load "../input/day19.input"
  print ("day19, part1 (sam)", check 19114 $ part1 sam)
  print ("day19, part1", check 382440 $ part1 inp)
  print ("day19, part2 (sam)", check 167409079868000 $ part2 sam)
  print ("day19, part2", check 136394217540123 $ part2 inp)

gram :: Par Spec
gram = do
  xs <- terminated nl namedWorkflow
  nl
  ys <- separated nl part
  pure (Spec (Map.fromList xs) ys)

  where
    namedWorkflow :: Par (Name,Workflow)
    namedWorkflow = do
      x <- name
      lit '{'
      rs <- terminated (lit ',') rule
      y <- name
      lit '}'
      pure (x,Workflow rs y)

    rule :: Par Rule
    rule = do
      c <- cond
      lit ':'
      n <- name
      pure (c,n)

    name :: Par Name
    name = word

    cond :: Par Cond
    cond = noError $ do
      c <- cat
      o <- op
      v <- int
      pure (c,o,v)

    cat :: Par Cat
    cat = alts [ do lit 'x'; pure X
               , do lit 'm'; pure M
               , do lit 'a'; pure A
               , do lit 's'; pure S ]

    op :: Par Op
    op = alts [ do lit '<'; pure Less
              , do lit '>'; pure Greater ]

    part :: Par Part
    part = do
      lit '{'
      x <- rating 'x'
      lit ','
      m <- rating 'm'
      lit ','
      a <- rating 'a'
      lit ','
      s <- rating 's'
      lit '}'
      pure (Part {x,m,a,s})

    rating :: Char -> Par Int
    rating c = do
      lit c
      lit '='
      int

data Spec = Spec (Map Name Workflow) [Part] deriving Show
data Workflow = Workflow [Rule] Name deriving Show
type Rule = (Cond,Name)
type Name = String
type Cond = (Cat,Op,Int)
data Op = Less | Greater deriving Show
data Cat = X | M | A | S deriving Show
data Part = Part { x :: Int, m :: Int, a :: Int, s :: Int } deriving Show

value :: Part -> Int
value Part {x,m,a,s} = x+m+a+s

part1 :: Spec -> Int
part1 (Spec defs parts) = sum [ value p | p <- parts, check p ]

  where
    check :: Part -> Bool
    check Part{x,m,a,s} = runName "in"
      where
        evalCat :: Cat -> Int
        evalCat = \case X -> x; M -> m; A -> a; S -> s

        runName :: Name -> Bool
        runName name =
          case name of "A" -> True; "R" -> False; _ -> runWorkflow (look name defs)

        runWorkflow :: Workflow -> Bool
        runWorkflow (Workflow rs def) = loop rs
          where
            loop :: [Rule] -> Bool
            loop = \case
              [] -> runName def
              (cond,name):xs ->
                if runCond cond then runName name else loop xs

        runCond :: Cond -> Bool
        runCond (c,o,v) = evalCat c `op` v
          where op = case o of Less -> (<); Greater -> (>)

part2 :: Spec -> Int
part2 (Spec defs _) = sum [ sizeR4 r4 | r4 <- runName "in" maxR4 ]
  where
    runName :: Name -> R4 -> [R4]
    runName name r4 =
      case name of "A" -> [r4]; "R" -> []; _ -> runWorkflow (look name defs) r4

    runWorkflow :: Workflow -> R4 -> [R4]
    runWorkflow (Workflow rs def) = loop rs
      where
        loop :: [Rule] -> R4 -> [R4]
        loop rs r4 = case rs of
          [] -> runName def r4
          (cond,name):xs -> do
            let (yes,no) = splitR4 r4 cond
            runName name yes ++ loop xs no

data R4 = R4 { rx :: R, rm :: R, ra :: R, rs :: R } deriving Show
data R = R { low :: Int, high :: Int }

instance Show R where
  show R {low,high} = show low ++ ".." ++ show high

maxR4 :: R4
maxR4 = R4 { rx = maxR, rm = maxR, ra = maxR, rs = maxR }

maxR :: R
maxR = R { low = 1, high = 4000 }

sizeR4 :: R4 -> Int
sizeR4 R4 {rx,rm,ra,rs} = sizeR rx * sizeR rm * sizeR ra * sizeR rs

sizeR :: R -> Int
sizeR R {low,high} = high-low+1

splitR4 :: R4 -> Cond -> (R4,R4)
splitR4 r4 (cat,o,v) = do
  let (yes,no) = splitR (get r4 cat) (o,v)
  (set yes cat r4, set no cat r4)
  where
    get :: R4 -> Cat -> R
    get R4{rx,rm,ra,rs} = \case X -> rx; M -> rm; A -> ra; S -> rs

    set :: R -> Cat -> R4 -> R4
    set r = \case
      X -> \q -> q { rx = r }
      M -> \q -> q { rm = r }
      A -> \q -> q { ra = r }
      S -> \q -> q { rs = r }

splitR :: R -> (Op,Int) -> (R,R)
splitR R{low,high} (o,v) = (yes,no)
  where
    yes = case o of
      Less -> R { low, high = min high (v-1) }
      Greater -> R { low = max low (v+1), high }

    no = case o of
      Less -> R { low = max low v, high }
      Greater -> R { low, high = min high v }
