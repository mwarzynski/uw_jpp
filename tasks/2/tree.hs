-- Haskell Laboratory 2. --
module Tree (Tree(Node)) where

-- Tree definition -- 
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Ord)

-- A)
-- create a implementation of Eq and Show.
instance Show a => Show (Tree a) where
    show (Node root left right) = (show root) ++ "(LEFT(" ++ (show left) ++ ") " ++
                                  "RIGHT(" ++ (show right) ++ ")) " 
    show Empty = ""

instance Eq a => Eq (Tree a) where
    (Node n1 l1 r1) == (Node n2 l2 r2) = (n1 == n2 && l1 == l2 && r1 == r2)
    Empty == Empty = True
    _ == _ = False

-- B)
-- instance Functor Tree where...
instance Functor Tree where  
    fmap f Empty = Empty
    fmap f (Node root left right) = Node (f root) (fmap f left) (fmap f right)  

-- C)
-- Write function:
toList :: Tree a -> [a]
-- which generates list of nodes in infix order
toList Empty = []
toList (Node n l r) = (toList l) ++ [n] ++ (toList r)

-- D)
-- implement BST trees with given functions:
insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node n l r) = if x < n then
                            (Node n (insert x l) r)
                        else
                            (Node n l (insert x r))

contains :: (Ord a) => a -> Tree a -> Bool
contains x Empty = False
contains x (Node n l r) = if x == n then True else
                          if x < n then contains x l else contains x r

fromList :: (Ord a) => [a] -> Tree a
fromList [] = Empty
fromList (h:t) = insert h (fromList t)

-- E)
-- create BST trees module and use it in another module to sort [Int]
-- E.1) using ghci
-- E.2) using ghc --make

