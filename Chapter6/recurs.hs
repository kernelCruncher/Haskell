import System.Directory.Internal.Prelude (Num)
fac :: Int -> Int
fac 0 = 1
fac n   | n>0 = n * fac (n-1)

sumdown :: Int -> Int
sumdown 0 = 0
sumdown m = m + sumdown (m-1)

(^) :: Int -> Int -> Int
m ^ 0 = 1
m ^ n = m * (m Main.^ (n-1))

euclid :: Int -> Int -> Int
euclid n m  | n == m = m 
            | n > m = euclid (n - m) m
            | otherwise = euclid (m - n) n

-- drop (drops first n elements from list)
-- init removes final element from list

and :: [Bool] -> Bool
and [] = True 
and (x : xs) = x && (Main.and xs)

concatenate :: [[a]] -> [a] --concat flattens a list of lists into a single list, e.g. [[1,2], [3]] becomes [1,2,3]
concatenate [] = []
concatenate (x : xs) = x <> concatenate xs

replicate :: Int -> a -> [a]
replicate 1 x = [x]
repicate m x = x : (Main.replicate (m-1) x)

(!!) :: [a] -> Int -> a
(x : xs) !! 0 = x
(x : xs) !! m = xs Main.!! (m-1)

elem :: Eq a => a -> [a] -> Bool 
elem a [] = False 
elem a (x : xs) | a == x = True 
                | otherwise = Main.elem a xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys) | x >y = y : x : merge xs ys
                        | otherwise = x: y : merge xs ys  

halve :: [a] -> ([a], [a])
halve xs = (Prelude.take m xs , drop m xs)
    where m = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort zs = merge firstHalfSorted secondHalfSorted
     where firstHalfSorted  = msort . fst $ halves
           secondHalfSorted = msort . snd $ halves
           halves = halve zs

sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + Main.sum xs

take :: [a] -> Int -> [a]
take xs 0 = []
take (x : xs) m = [x] ++ Main.take xs (m-1)

last :: [a] -> a
last [x] = x
last (x : xs) = Main.last xs