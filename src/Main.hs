module Main where


letters xs = [take n xs | n <- [0..length xs]]

dists xs [] = reverse $ letters xs
dists [] = letters
dists X@(x:xs) Y@(y:ys) =
  minimumBy length  [X : dists xs Y,
                     (dists X ys) ++ [Y]
                     X' ++ (dists xs ys)]
    where X' = if x==y [] else [X]

