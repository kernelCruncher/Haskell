data Nat = Zero | Succ Nat

nat2int :: Nat -> Int 
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add m n = int2nat (nat2int m + nat2int n)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero 
mult m (Succ n) = add m (mult m n)

data Tree a = Leaf a | Node (Tree a) a (Tree a)

-- occurs :: Ord a => a -> Tree a -> Bool 
-- occurs a tree = compare