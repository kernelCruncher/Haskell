module Expression where

import Operation ( Op, valid, apply )

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n)    = show n
    show (App o l r) = brak l ++ show o ++ brak r
                    where
                        brak (Val n) = show n
                        brak e   = "(" ++ show e ++ ")"
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n> 0]
eval (App o l r) = [apply o x y | x <- eval l,y <- eval r, valid o x y]

--Subs returns all possible subsequences of a list.
subs :: [a] -> [[a]]
subs []     = [[]]
subs (x : xs) = yss ++ map (x:) yss 
    where yss = subs xs