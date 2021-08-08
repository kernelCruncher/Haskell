import Data.List(sort)

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

--First Past the Post voting system
count :: Eq a => a -> [a] -> Int 
count x = length . filter (==x)

rmdups :: Eq a => [a] -> [a] --removes duplicate values
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

--Alternative voting system
ballots :: [[String]]
ballots = [["Red", "Green"],
            ["Blue"],
            ["Green", "Red", "Blue"],
            ["Blue", "Green", "Red"],
            ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a] -- rank from lowest to highest
rank = map snd . result . map head -- snd takes seconf value of a tuple

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
                [c] -> c
                (c : cs) -> winner' (elim c bs)

