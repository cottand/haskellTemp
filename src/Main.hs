module Main where

main :: IO ()
main = putStr $ show (dists "hello" "bhell")

letters xs = [take n xs | n <- [0 .. length xs]]

dists :: String -> String -> [String]
dists xs [] = reverse $ letters xs
dists [] xs = letters xs
dists x'@(x:xs) y'@(y:ys) = minimumBy length [x' : dists xs y', dists x' ys ++ [y'], ds']
  where
    ds = dists xs ys
    ds' =
      if x == y
        then map (x :) ds
        else x' : map (y :) ds
    minimumBy :: (a -> Int) -> [a] -> a
    minimumBy _ [x] = x
    minimumBy f (x:y:xs) =
      if f x > f y
        then minimumBy f (y : xs)
        else minimumBy f (x : xs)