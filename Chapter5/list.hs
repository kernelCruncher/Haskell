module Exercises where

import Prelude

exercise1 :: Int 
exercise1 = sum [i^2 | i <- [1..100]]

grid :: Int -> Int -> [(Int, Int)]
grid x y = [(i,j) | i <- [0..x], j <- [0..y]]

square :: Int -> [(Int, Int)]
square x = [(i,j) | (i, j) <- grid x x, i /= j]

replicate :: Int -> a -> [a]
replicate n y = [y | _ <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths x = [(i,j,k) | i <- [1..x], j <- [1..x], k <- [1..x], i^2 + j^2 == k^2 ]

perfects :: Int -> [Int]
perfects n = [m | m <- [1..n], sum (factorsMinus m) == m]
    where 
        factors :: Int -> [Int]
        factors n = [x | x <- [1 ..n], n `mod` x == 0]
        factorsMinus :: Int -> [Int]
        factorsMinus m = [n | n <- factors m, n /= m]

exercise7 :: [(Int, Int)]
exercise7 = concat [[(x, y) | y <- [3, 4]] | x <- [1, 2]] -- this has two comprehensions

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x $ zip xs [1..]
    where
        find :: Eq a => a -> [(a,b)] -> [b]
        find k t = [v | (k',v) <- t, k == k'] -- this returns a list of all values that are associated with a given key.

scalarproduct :: [Int] -> [Int] -> Int 
scalarproduct xs ys = sum [xi * yi | (xi, yi) <- zip xs ys]

