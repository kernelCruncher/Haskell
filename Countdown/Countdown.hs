module Main where -- need this to be Main if we want o produce an executable

import Expression ( Expr(..), values, eval, subs )
import Operation ( Op(..), valid, apply )

main :: IO ()
main = print (solutions [1,3,7,10,25,50] 765)

--Interleave returns all possible ways of inserting a new element into a list.
interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys) -- : is infix for the prepend operator

--perms returns all possible ways of permuting a list.
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]] -- How to get all possible choices of a list, i.e. all permutations of subsequences
choices = concat . map perms . subs -- using point-free notation. Same as concat (map Perms (subs a)). Recall subs returns all possible subsequences of a list and perms returns all possible permutations

-- Alternative for Choices using List Comprehension
-- choices :: [a] -> [[a]]
-- choices lst = [ys | xs <- subs lst, ys <- perms xs]

solution :: Expr -> [Int] -> Int -> Bool 
solution e ns n =
    elem (values e) (choices ns) && eval e == [n]

--Brute force solution
split :: [a] -> [([a], [a])]
split []    = []
split [_]   = []
split (x:xs) = ([x], xs) : [(x:ls,rs) | (ls,rs) <- split xs]

--Exprs 
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

--Attempt 2
type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n) | n >0]
results ns = [res | (ls,rs) <- split ns, lx <- results ls, ry <- results rs, res <- combine' lx ry]
    where 
        combine' :: Result -> Result -> [Result]
        combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]