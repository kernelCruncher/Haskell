--map f fiter (p xs)

-- all :: (a -> Bool) -> [Bool] -> Bool 
-- all f 

map :: (a -> b) -> [a] -> [b]
map f = foldr (\m acc -> f m : acc) []

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\m acc -> if f m then m : acc else acc) []

dec2int :: [Int] -> Int
dec2int = foldl(\x acc -> 10*acc + x) 0

curry :: ((a,b) -> c) -> (a -> b -> c)
curry f = \x y -> f (x, y)

uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f = \(x, y) -> f x y

