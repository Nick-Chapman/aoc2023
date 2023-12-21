
module Day20 (main) where

import Data.Map (Map)
import Misc (check,collate,look)
import Par4 (Par,parse,separated,nl,alts,lit,word,key)
import qualified Data.Map as Map

gram :: Par Spec
gram = Spec <$> separated nl line
  where
    line :: Par Line
    line = do
      (t,n) <- alts [ do key "broadcaster"; pure (Broadcaster,"broadcaster")
                    , do lit '%'; n <- word; pure (FF,n)
                    , do lit '&'; n <- word; pure (Conj,n)
                    ]
      key " -> "
      ns <- separated (key ", ") word
      pure (t,n,ns)

main :: IO ()
main = do
  let load x = parse gram <$> readFile x
  samA <- load "../input/day20-sample1.input"
  samB <- load "../input/day20-sample2.input"
  inp <- load "../input/day20.input"
  a <- part1 samA
  print ("day20, part1 (samA)", check 32000000 $ a)
  b <- part1 samB
  print ("day20, part1 (samB)", check 11687500 $ b)
  r <- part1 inp
  print ("day20, part1", check 821985143 $ r)

data Spec = Spec [Line] deriving Show
type Line = (Type,Name,[Name])
data Type = FF | Conj | Broadcaster deriving Show
type Name = String

part1 :: Spec -> IO Int
part1 (Spec lines) =  do
  sFinal <- run 0 initS
  let State{l,h} = sFinal
  pure (l * h)

  where
    run :: Int -> State -> IO State
    run n s = do
        --print ("run",n)
        if n == 1000 then pure s else do
          s' <- stabilize 0 (pushButton s)
          run (n+1) s'

    srcsByDest :: Map Name [Name] = Map.fromList $ collate [ (d,s) | (_,s,ds) <- lines, d <- ds ]

    pushButton :: State -> State
    pushButton s@State{q,l} = s { q = enQ q ("button","broadcaster",False), l = l+1 }

    initS :: State
    initS = State { m, q , h = 0, l = 0 }
      where
        q = emptyQ
        m = Map.fromList
          [
            (d,ms)
            | (t,d,_) <- lines
            , let ms = case t of
                    Broadcaster -> SBroadcaster
                    FF -> SFF False
                    Conj -> SConj (Map.fromList [ (s,False) | s <- look d srcsByDest ])
          ]

    destsOf :: Name -> [Name]
    destsOf = flip look m
      where m = Map.fromList $ [ (name,ds) | (_,name,ds) <- lines ]

    stabilize :: Int -> State -> IO State
    stabilize i s@State{m,q,h,l} = do
      case deQ q of
        Nothing -> do
          pure s
        Just (_p@(from,dest,vIn),q) -> do
          case Map.lookup dest m of
            Nothing -> do
              if dest == "rx" && not vIn then error "STOP" else
                stabilize (i+1) s { q }
            Just ms -> do
              case ms of
                SBroadcaster -> do
                  let m' = m
                  let vOut = vIn
                  let ps = [ (dest,d,vOut) | d <- destsOf dest ]
                  let n = length ps
                  let q' = foldl enQ q ps
                  let l' = if vOut then l else n + l
                  let h' = if vOut then n + h else h
                  let s' = s { m = m', q = q', h = h', l = l' }
                  stabilize (i+1) s'

                SFF oldV -> do
                  if vIn then stabilize (i+1) s { q } else do
                    let vOut = not oldV
                    let m' = Map.insert dest (SFF vOut) m
                    let ps = [ (dest,d,vOut) | d <- destsOf dest ]
                    let n = length ps
                    let q' = foldl enQ q ps
                    let l' = if vOut then l else n + l
                    let h' = if vOut then n + h else h
                    let s' = s { m = m', q = q', h = h', l = l' }
                    stabilize (i+1) s'

                SConj remembered -> do
                  let remembered' = Map.insert from vIn remembered
                  let m' = Map.insert dest (SConj remembered') m
                  let vOut = not (all id (Map.elems remembered'))
                  let ps = [ (dest,d,vOut) | d <- destsOf dest ]
                  let n = length ps
                  let q' = foldl enQ q ps
                  let l' = if vOut then l else n + l
                  let h' = if vOut then n + h else h
                  let s' = s { m = m', q = q', h = h', l = l' }
                  stabilize (i+1) s'

data State = State { m :: Map Name MS, q :: Q Pulse, h :: Int, l :: Int }
  deriving Show

data MS = SFF Bool | SConj (Map Name Bool) | SBroadcaster deriving Show

type Pulse = (Name,Name,Bool)

data Q a = Q [a] [a] deriving Show

emptyQ :: Q a
emptyQ = Q [] []

enQ :: Q a -> a -> Q a
enQ (Q fs bs) b = Q fs (b:bs)

deQ :: Q a -> Maybe (a,Q a)
deQ (Q fs bs) =
  case fs of
    f:fs -> Just (f, Q fs bs)
    [] -> case (reverse bs) of
      f:fs -> Just (f, Q fs [])
      [] -> Nothing
