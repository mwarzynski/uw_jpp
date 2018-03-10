-- Haskell Laboratory 2. --

import Prelude hiding (Either(..))

data Either a b = Left a | Right b deriving Show

-- A)
-- write instances of Functor class for Either e:
instance Functor (Either e) where
    fmap f (Left a) = (Left a)
    fmap f (Right b) = Right (f b)

-- B)
-- write function reverseRight
reverseRight :: Either e [a] -> Either e [a]
-- which reverses list included in Right (with using fmap and without)
-- without fmap
reverseRight (Left a) = Left a
reverseRight (Right b) = Right (reverse b)
-- with    fmap
reverseRightF (Left a) = Left a
reverseRightF b = fmap (reverse) b

-- C)
-- define Pointed class (function containers with singleton)
class Functor f => Pointed f where
	pure :: a -> f a
-- and it's instances for list, Maybe and Tree

-- what the fuck am I supposed to code here?

