{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

module Main where

import           GHC.Arr       (Array, array, listArray, (!))

import           Data.Foldable (maximumBy)
import           Data.Ix       (Ix, range)
import           Lib

main :: IO ()
main = putStr $ show (dists "hello" "bhell")

letters xs = [take n xs | n <- [0 .. length xs]]

dists :: String -> String -> [String]
dists xs [] = reverse $ letters xs
dists [] xs = letters xs
dists xs'@(x:xs) ys'@(y:ys) = minimumBy length [xs' : dists xs ys', dists xs' ys ++ [ys'], ds']
  where
    ds = dists xs ys
    ds'
      | x == y = map (x :) ds
      | otherwise = xs' : map (y :) ds
    minimumBy _ [x] = x
    minimumBy f (x:y:xs)
      | f x > f y = minimumBy f (y : xs)
      | otherwise = minimumBy f (x : xs)

fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibt :: Int -> Integer
fibt n = t ! n
  where
    t = tabulate (0, n) f
    f 0 = 0
    f 1 = 1
    f m = t ! (m - 1) + t ! (m - 2)

fib' n = h n 0 0
  where
    h 0 a _ = a
    h n a b = h (n - 1) (a + b) a

maximum :: Ord a => a -> [a] -> a
maximum = foldr max

fromList [] = listArray (0, 0) []
fromList xs = listArray (0, length xs - 1) xs

data SA =
  SA (Array Int Int) Int Int

pop :: SA -> SA
empty = SA (fromList []) 0 0

pop (SA _ 0 _)   = empty
pop (SA arr s c) = SA arr (s - 1) c

peek (SA _ 0 _) = Nothing

--peek sa = look 0 sa
--push x (SA arr s c)
--  | s < c
--    = SA (modify arr s x) (s+1) c
knapsack wvs c = Main.maximum 0 [v + knapsack wvs (c - w) | (_, w, v) <- wvs, w <= c]

knapsack' ::
     forall name weight value. (Ix weight, Num weight, Ord value, Num value)
  => [(name, weight, value)]
  -> weight
  -> value
knapsack' wvs c = table ! c
  where
    table :: Array weight value
    table = tabulate (0, c) mk
    mk :: weight -> value
    mk c = Main.maximum 0 [v + table ! (c - w) | (_, w, v) <- wvs, w <= c]

knapsack'' ::
     forall name weight value. (Ix weight, Num weight, Ord value, Num value)
  => [(name, weight, value)]
  -> weight
  -> (value, [name])
knapsack'' wvs c = table ! c
  where
    table :: Array weight (value, [name])
    table = tabulate (0, c) mknapsack
    mknapsack :: weight -> (value, [name])
    mknapsack c
      | null ts = (0, [])
      | otherwise = maximumBy ord ts
    ord (v1, _) (v2, _) = compare v1 v2
    ts = [(v + v', n : ns) | (n, w, v) <- wvs, let (v', ns) = table ! (c - w), w <= c]
