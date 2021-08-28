import Prelude

halve :: [a] -> ([a], [a]) --div gives integer but \ gives exact division
halve xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs ) -- drop removes first elements from list

third :: [a] -> a
third = head . tail . tail

third2 :: [a] -> a
third2 xs = xs !! 2

third3 :: [a] -> a
third3 (_ : _ :x : _) = x

safeTail :: [a] -> [a]
safeTail xs  =  if null xs
                    then []
                else 
                    tail xs

safeTail2 :: [a] -> [a]
safeTail2 xs    | null xs  = []    
                | otherwise = tail xs

safeTail3 :: [a] -> [a]
safeTail3 [] = []
safeTail3 (_ : xs) = xs

(||) :: Bool -> Bool -> Bool 
True || True = True 
True || False = True 
False || True = True 
False || False = False

(&&) :: Bool -> Bool -> Bool 
a && b = if a
            then 
                if b
                    then True 
                else
                    False
        else 
            False

-- (&&) :: Bool -> Bool -> Bool 
-- a && b = if a
--             then 
--                 b
--         else 
--             False      

mult :: Int -> Int -> Int -> Int
mult = (\x -> (\y -> (\z -> x*y*z)))

luhnDouble :: Int -> Int
luhnDouble x =  if 2 * x < 9
                    then 2 * x
                else
                    2 * x - 9

luhn :: Int -> Int -> Int -> Int -> Bool 
luhn i j k l = ( l + j + sum (map luhnDouble [i,k])) `mod` 10 == 0

