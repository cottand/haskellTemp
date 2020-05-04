module Lib
  ( tabulate
  , someFunc
  ) where

import           GHC.Arr (Array, array, (!))

import           Data.Ix (Ix, range)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

tabulate :: Ix i => (i, i) -> (i -> a) -> Array i a
tabulate (u, v) f = array (u, v) [(i, f i) | i <- range (u, v)]
