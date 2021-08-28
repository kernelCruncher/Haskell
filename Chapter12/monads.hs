data Tree a = Leaf | Node (Tree a ) a (Tree a)
            deriving Show

instance Functor Tree where
    fmap f (Leaf) = Leaf
    fmap f (Node x y z) = Node (fmap f x) (f y) (fmap f z) -- note we can't use Tree in the definion as Tree is a Haskell data type and not a constructor.

instance Functor ((->) a) where
    --fmap :: (b-> c) -> (a -> b) -> (a -> c) 
    fmap = (.)

instance Applicative ((->) a) where
    pure = const
    g <*> h = \x -> g . h


