\documentclass[a4]{tufte-handout}
% The documentclass can be changed, but keep fonts at a reasonable size.

% comments
\usepackage{comment}

% code environments
\usepackage{listings}
\lstnewenvironment{code}{
  \lstset{language=haskell, basicstyle=\ttfamily }}{}
\lstnewenvironment{spec}{
  \lstset{language=haskell, basicstyle=\ttfamily }}{}
\lstset{language=haskell, basicstyle=\ttfamily }


\title{CO202: Coursework 1}
\date{Autumn Term, 2019}
\author{Group \#number}


\begin{document}
\maketitle

The source of this document is \texttt{Submision.lhs}, and should form the
basis of your report as well as contain all the code for your submission. You
should remove text (such as all the text in this section) that is here for your
information only and that does not contribute to your submission.
You should start by modifying the \verb|\author{}| command above to include
your group number.

The source code of the provided \texttt{Submission.lhs} contains code and
comments that are hidden from the final \texttt{pdf} file, so you should
inspect it carefully.  For instance, the code declares the use of various
language features that are used in this code base.  You can learn more about
these language features in the language extensions section of the GHC
documentation at
\url{https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html}
if you wish, but for the most part you need not worry about them.
\begin{comment}
The code in commented blocks such as this one is required for this file to
compile.
\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
\end{code}
\end{comment}

The following imports various modules that are used. You should avoid depending
on any libraries other than those distributed with GHC:
\href{http://hackage.haskell.org/package/base}{\texttt{base}} and
\href{https://hackage.haskell.org/package/containers}{\texttt{containers}}
ought to contain everything you need.
\begin{comment}
\begin{code}
module Submission where

import Prelude hiding (maximum)
import Data.Maybe (fromJust)
import Data.Coerce (coerce)
import Data.Function (on)

import Data.Array
import Data.List (nub, sortBy, maximumBy, minimumBy, tails, inits, mapAccumL, (\\))
import Data.Map (Map)
import qualified Data.Map as M

\end{code}
\end{comment}

All of the necessary types and definitions from the specification of this
coursework have been given to you in the source of this document. You need not repeat
that code in your submission, but it is required within the \verb|\begin{code}| and \verb|\end{code}| markers so that it can be compiled.

Before submitting your coursework, you should ensure that your code compiles
properly. Use the following command with the supplied
\texttt{Submission.lhs-boot} file to check that it can be marked:
\begin{spec}
ghc -fforce-recomp -c Submission.lhs-boot Submission.lhs
\end{spec}
This checks to see if all the type signatures of exposed functions are as
expected.


\begin{comment}
\begin{code}
data Player = Player1 | Player2
data Planet = Planet Owner Ships Growth
newtype Ships = Ships Int
newtype Growth = Growth Int
data Owner = Neutral | Owned Player
newtype PlanetId = PlanetId Int
type Planets = Map PlanetId Planet
data Wormhole = Wormhole Source Target Turns

newtype Source = Source PlanetId
newtype Target = Target PlanetId
newtype Turns  = Turns Int
newtype WormholeId = WormholeId Int
type Wormholes = Map WormholeId Wormhole
data Fleet = Fleet Player Ships WormholeId Turns
type Fleets = [Fleet]
data GameState = GameState Planets Wormholes Fleets
data Order = Order WormholeId Ships
\end{code}
\end{comment}

\begin{comment}
\begin{code}
fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

fib' :: Int -> Integer
fib' n = table ! n
  where
    table :: Array Int Integer
    table = tabulate (0, n) mfib

    mfib 0 = 0
    mfib 1 = 1
    mfib n = table ! (n-1) + table ! (n-2)

tabulate :: Ix i => (i,i) -> (i -> a) -> Array i a
tabulate (u,v) f = array (u,v) [ (i, f i) | i <- range (u, v)]
\end{code}
\end{comment}

\begin{comment}
\begin{code}
example1 :: GameState
example1 = GameState planets wormholes fleets where
  planets = M.fromList
    [ (PlanetId 0, Planet (Owned Player1) (Ships 300) (Growth 0))
    , (PlanetId 1, Planet Neutral         (Ships 200) (Growth 50))
    , (PlanetId 2, Planet Neutral         (Ships 150) (Growth 10))
    , (PlanetId 3, Planet Neutral         (Ships 30)  (Growth 5))
    , (PlanetId 4, Planet Neutral         (Ships 100) (Growth 20))
    ]
  wormholes = M.fromList
    [ (WormholeId 0, Wormhole homePlanet (Target 1) (Turns 1))
    , (WormholeId 1, Wormhole homePlanet (Target 2) (Turns 1))
    , (WormholeId 2, Wormhole homePlanet (Target 3) (Turns 1))
    , (WormholeId 3, Wormhole homePlanet (Target 4) (Turns 1))
    ] where homePlanet = Source 0
  fleets = []

targetPlanets :: GameState -> Source -> [(PlanetId, Ships, Growth)]
targetPlanets st s
  = map (planetDetails . target) (M.elems (wormholesFrom s st))
  where
    planetDetails :: PlanetId -> (PlanetId, Ships, Growth)
    planetDetails pId = (pId, ships, growth)
      where Planet _ ships growth = lookupPlanet pId st

shipsOnPlanet :: GameState -> PlanetId -> Ships
shipsOnPlanet st pId = ships
  where Planet _ ships _ = lookupPlanet pId st

lookupPlanet :: PlanetId -> GameState -> Planet
lookupPlanet pId (GameState ps _ _) = fromJust (M.lookup pId ps)

wormholesFrom :: Source -> GameState -> Wormholes
wormholesFrom pId (GameState _ ws _)
  = M.filter (\(Wormhole s _ _) -> s == pId) ws

wormholesTo :: Target -> GameState -> Wormholes
wormholesTo pId (GameState _ ws _)
  = M.filter (\(Wormhole _ t _) -> t == pId) ws

knapsack :: (Ord weight, Num weight, Ord value, Num value) =>
  [(name, weight, value)] -> weight -> value
knapsack wvs c = maximum 0 [ v + knapsack wvs (c - w) | (_,w,v) <- wvs , w <= c ]

maximum :: Ord a => a -> [a] -> a
maximum x xs = foldr max x xs
\end{code}
\end{comment}

\marginnote{Make sure that the problems you are solving are clearly indicated.
Using a section is a good idea. You should endeavor to concisely explain the
code you have written. Feel free to make use of your own margin notes, and do
please remove this one.}
\section*{Problem 1: Dynamic Knapsack}

\begin{code}
knapsack' :: forall name weight value .
  (Ix weight, Num weight, Ord value, Num value) =>
  [(name, weight, value)] -> weight -> value
knapsack' wvs c = table ! c
  where
    table :: Array weight value
    table = tabulate (0,c) mknapsack

    mknapsack :: weight -> value
    mknapsack c = undefined
\end{code}

\section*{Problem 2: Knapsack Elements}

\begin{code}
knapsack'' :: forall name weight value .
  (Ix weight, Num weight, Ord value, Num value) =>
  [(name, weight, value)] -> weight -> (value, [name])
knapsack'' wvs c = table ! c
  where
    table :: Array weight (value, [name])
    table = tabulate (0,c) mknapsack

    mknapsack :: weight -> (value, [name])
    mknapsack c = undefined
\end{code}

\section*{Problem 3: Bounded Knapsack}
\begin{code}
bknapsack
  :: (Ord weight, Num weight, Ord value, Num value)
  => [(name, weight, value)] -> weight -> (value, [name])
bknapsack = undefined
\end{code}

\section*{Problem 4: Reasonable Indexes}

\section*{Problem 5: Bounded Knapsack Revisited}

\begin{code}
bknapsack' :: forall name weight value .
  (Ord weight, Num weight, Ord value, Num value) =>
  [(name, weight, value)] -> Int ->
  weight -> (value, [name])
bknapsack' = undefined
\end{code}

\section*{Problem 6: Dynamic Bounded Knapsack}

\begin{code}
bknapsack'' :: forall name weight value .
  (Ord name, Ix weight, Ord weight, Num weight,
    Ord value, Num value) =>
  [(name, weight, value)] -> weight -> (value, [name])
bknapsack'' = undefined
\end{code}

\section*{Problem 7: Dijkstra Dualized}

\begin{comment}
\begin{code}
optimise :: GameState -> Source -> (Growth, [PlanetId])
optimise st s@(Source p) = bknapsack'' (targetPlanets st s) (shipsOnPlanet st p)

type Weight = Integer

class Eq v => Edge e v | e -> v where
  source :: e -> v
  target :: e -> v
  weight :: e -> Weight

instance Edge (String, String, Integer) String where
  source (s, _, _) = s
  target (_, t, _) = t
  weight (_, _, i) = i

instance Edge Wormhole PlanetId where
  source (Wormhole (Source s) _ _)    = s
  target (Wormhole _ (Target t) _)    = t
  weight (Wormhole _ _ (Turns turns)) = toInteger turns

instance Edge (WormholeId, Wormhole) PlanetId where
  source (_, w) = source w
  target (_, w) = target w
  weight (_, w) = weight w

data Path e = Path Weight [e]
\end{code}

\begin{code}
pathFromEdge :: Edge e v => e -> Path e
pathFromEdge e = Path (weight e) [e]
\end{code}

\begin{code}
extend :: Edge e v => Path e -> e -> Path e
extend (Path _ []) _ = error "extend: Empty path"
extend (Path d (e:es)) e'
  | target e == source e' = Path (d + weight e') (e':e:es)
  | otherwise = error "extend: Incompatible endpoints"
\end{code}

\begin{code}
pathFromEdges :: Edge e v => [e] -> Path e
pathFromEdges (x : xs) = foldl extend (pathFromEdge x) xs
pathFromEdges [] = error "pathFromEdges: Empty list of edges"
\end{code}

\begin{code}
instance Edge e v => Edge (Path e) v where
  source (Path _ es) = source (last es)
  target (Path _ es) = target (head es)
  weight (Path w _)  = w
\end{code}

\begin{code}
class Edge e v => Graph g e v | g -> e where
  vertices  :: g -> [v]
  edges     :: g -> [e]
  edgesFrom :: g -> v -> [e]
  edgesTo   :: g -> v -> [e]
  velem     :: v -> g -> Bool
  eelem     :: e -> g -> Bool
\end{code}

\begin{code}
instance (Eq e, Edge e v) => Graph [e] e v where
  vertices es = nub (map source es ++ map target es)
  edges es    = es
  edgesFrom es v = [ e | e <- es, v == source e ]
  edgesTo   es v = [ e | e <- es, v == target e ]
  velem v es = v `elem` vertices es
  eelem v es = v `elem` edges es
\end{code}

\begin{code}
example2 :: [(String, String, Integer)]
example2 = [("s","t",10), ("s","y",5), ("t","x",1), ("t","y",2), ("y","t",3),
            ("y","x", 9), ("x","z",4), ("z","x",6), ("y","z",2), ("z","s",7)]
\end{code}

\begin{code}
instance Graph GameState (WormholeId, Wormhole) PlanetId where
  vertices (GameState ps _ _) = M.keys ps
  edges    (GameState _ ws _) = M.assocs ws
  edgesTo   st pId = M.toList (wormholesTo (Target pId) st)
  edgesFrom st pId = M.toList (wormholesFrom (Source pId) st)
  velem pId      (GameState ps _ _) = M.member pId ps
  eelem (wId, _) (GameState _ ws _) = M.member wId ws
\end{code}
\end{comment}

\begin{comment}
\begin{code}
lte :: (a -> a -> Ordering) -> (a -> a -> Bool)
lte cmp x y = cmp x y /= GT

eq :: (a -> a -> Ordering) -> (a -> a -> Bool)
eq cmp x y = cmp x y == EQ
\end{code}

\begin{code}
class PQueue pqueue where
  toPQueue   :: (a -> a -> Ordering) -> [a] -> pqueue a
  fromPQueue :: pqueue a -> [a]

  priority :: pqueue a -> (a -> a -> Ordering)

  empty :: (a -> a -> Ordering) -> pqueue a
  isEmpty :: pqueue a -> Bool

  insert :: a -> pqueue a -> pqueue a
  delete :: a -> pqueue a -> pqueue a

  extract :: pqueue a -> a
  discard :: pqueue a -> pqueue a
  detach  :: pqueue a -> (a, pqueue a)

data PList a = PList (a -> a -> Ordering) [a]

instance PQueue PList where

  toPQueue cmp xs = PList cmp (sortBy cmp xs)

  fromPQueue (PList _ xs) = xs

  empty cmp = PList cmp []

  isEmpty (PList _ xs) = null xs

  priority (PList cmp _) = cmp

  insert x (PList cmp []) = PList cmp [x]
  insert x ps@(PList cmp xs)
    | x <= y    = cons x ps
    | otherwise = cons y (insert x ys)
    where (<=) = lte cmp
          (y, ys) = detach ps
          cons x (PList cmp xs) = PList cmp (x:xs)

  delete x (PList cmp []) = PList cmp []
  delete x ps@(PList cmp _)
    | x == y    = ys
    | otherwise = cons y (delete x ys)
    where (==) = eq cmp
          (y, ys) = detach ps
          cons x (PList cmp xs) = PList cmp (x:xs)

  extract (PList cmp (x:xs)) = x

  discard (PList cmp (x:xs)) = PList cmp xs

  detach  (PList cmp (x:xs)) = (x, PList cmp xs)

cmpPath :: Path v -> Path v -> Ordering
cmpPath (Path d _) (Path d' _) = compare d d'
\end{code}
\end{comment}

\begin{comment}
\begin{code}
shortestPaths :: forall g e v. Graph g e v => g -> v -> [Path e]
shortestPaths g v = dijkstra g (vertices g \\ [v]) ps
 where
  ps :: PList (Path e)
  ps = foldr insert (empty cmpPath) (map pathFromEdge (edgesFrom g v))
\end{code}

\begin{code}
example3 :: GameState
example3 = GameState planets wormholes fleets where
  planets = M.fromList
    [ (PlanetId 0, Planet (Owned Player1) (Ships 300) (Growth 0))
    , (PlanetId 1, Planet Neutral         (Ships 200) (Growth 50))
    , (PlanetId 2, Planet Neutral         (Ships 150) (Growth 10))
    , (PlanetId 3, Planet Neutral         (Ships 30)  (Growth 5))
    , (PlanetId 4, Planet Neutral         (Ships 100) (Growth 20))
    , (PlanetId 5, Planet Neutral         (Ships 100) (Growth 20))
    ]
  wormholes = M.fromList
    [ (WormholeId 0, Wormhole homePlanet (Target 1) (Turns 1))
    , (WormholeId 1, Wormhole homePlanet (Target 2) (Turns 2))
    , (WormholeId 2, Wormhole homePlanet (Target 3) (Turns 3))
    , (WormholeId 3, Wormhole homePlanet (Target 4) (Turns 4))
    , (WormholeId 4, Wormhole (Source 4) (Target 5) (Turns 1))
    , (WormholeId 5, Wormhole (Source 2) (Target 5) (Turns 1))
    ] where homePlanet = Source 0
  fleets = []
\end{code}

\begin{code}
dijkstra :: (Graph g e v, PQueue pqueue) =>
  g -> [v] -> pqueue (Path e) -> [Path e]
dijkstra g [] ps = []
dijkstra g us ps
  | isEmpty ps  = []
  | v `elem` us = p : dijkstra g (us \\ [v])
                                 (foldr insert ps' (map (extend p) (edgesFrom g v)))
  | otherwise  = dijkstra g us ps'
  where
    (p, ps') = detach ps
    v = target p
\end{code}
\end{comment}

\section*{Problem 8: Heap Operations}


\begin{code}
data Heap a = Heap (a -> a -> Ordering) (Tree a)
data Tree a = Nil | Node Int (Tree a) a (Tree a)

instance PQueue Heap where
  toPQueue = undefined
  fromPQueue = undefined

  priority :: Heap a -> (a -> a -> Ordering)
  priority = undefined

  empty :: (a -> a -> Ordering) -> Heap a
  empty p = undefined

  isEmpty :: Heap a -> Bool
  isEmpty = undefined

  insert :: a -> Heap a -> Heap a
  insert = undefined

  delete :: a -> Heap a -> Heap a
  delete = undefined

  extract :: Heap a -> a
  extract = undefined

  discard :: Heap a -> Heap a
  discard = undefined

  detach :: Heap a -> (a, Heap a)
  detach = undefined
\end{code}

\begin{comment}
\begin{code}
shortestPaths' :: forall g e v . Graph g e v => g -> v -> [Path e]
shortestPaths' g v = dijkstra g (vertices g) ps
 where
  ps :: Heap (Path e)
  ps = foldr insert (empty cmpPath) (map pathFromEdge (edgesFrom g v))
\end{code}
\end{comment}

\section*{Problem 9: Adjacency List Graphs}

\begin{code}
newtype AdjList e v = AdjList [(v, [e])]

instance (Eq e, Edge e v) => Graph (AdjList e v) e v where
  vertices (AdjList ves)    = undefined
  edges (AdjList ves)       = undefined
  edgesFrom (AdjList ves) s = undefined
  edgesTo   (AdjList ves) t = undefined
  velem v (AdjList ves)     = undefined
  eelem e (AdjList ves)     = undefined
\end{code}

\section*{Problem 10: Conflict Zones}

\begin{code}
conflictZones :: GameState -> PlanetId -> PlanetId
  -> ([PlanetId], [PlanetId], [PlanetId])
conflictZones g p q = undefined
\end{code}

\begin{comment}
\begin{code}
deriving instance Show Player
deriving instance Read Player
deriving instance Show Owner
deriving instance Read Owner
deriving instance Show Planet
deriving instance Read Planet
deriving instance Show Fleet
deriving instance Read Fleet

deriving instance Show Wormhole
deriving instance Read Wormhole

deriving instance Show Order
deriving instance Read Order
deriving instance Show GameState
deriving instance Read GameState

deriving instance Ord PlanetId
deriving instance Eq PlanetId
deriving instance Num PlanetId
instance Show PlanetId where
  show (PlanetId x) = show x
instance Read PlanetId where
  readsPrec = coerce (readsPrec @Int)

deriving instance Ord Turns
deriving instance Eq Turns
deriving instance Num Turns
instance Show Turns where
  show (Turns x) = show x
instance Read Turns where
  readsPrec = coerce (readsPrec @Int)

deriving instance Ord Source
deriving instance Eq Source
instance Show Source where
  show (Source x) = show x
instance Read Source where
  readsPrec = coerce (readsPrec @Int)

deriving instance Num Growth
deriving instance Ord Growth
deriving instance Eq Growth
instance Show Growth where
  show (Growth x) = show x
instance Read Growth where
  readsPrec = coerce (readsPrec @Int)

deriving instance Ix Ships
deriving instance Num Ships
deriving instance Ord Ships
deriving instance Eq Ships
instance Show Ships where
  show (Ships x) = show x
instance Read Ships where
  readsPrec = coerce (readsPrec @Int)

deriving instance Ord Target
deriving instance Eq Target
instance Show Target where
  show (Target x) = show x
instance Read Target where
  readsPrec = coerce (readsPrec @Int)

deriving instance Eq WormholeId
deriving instance Ord WormholeId
instance Show WormholeId where
  show (WormholeId x) = show x
instance Read WormholeId where
  readsPrec = coerce (readsPrec @Int)

deriving instance Eq e   => Eq (Path e)
deriving instance Show e => Show (Path e)
instance Show a => Show (PList a) where
  show (PList _ xs) = show xs

\end{code}
\end{comment}

\end{document}