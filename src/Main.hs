module Main where

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
