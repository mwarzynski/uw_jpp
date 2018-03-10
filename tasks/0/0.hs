-- Haskell Laboratory 0. --

-- 1.
-- own implementation of head, tail, ++, take, drop
head1 (h:t) = h
tail1 (h:t) = t

concat1 [] l = l
concat1 (h:t) l = h:(concat1 t l)

take1 [] _ = []
take1 _ 0 = []
take1 (h:t) l = h:(take1 t (l-1)) 

drop1 x [] = []
drop1 x (h:t) = if x == h then drop1 x t else h:(drop1 x t)

-- 2.
-- inits, returns every initial list, example: [1, 2, 3] -> [[1], [1,2], [1,2,3]]
inits l = [take1 l i | i <- [0..length l]] -- well, complexity is not the best...

-- 3.
-- partitions, returns every pair of lists which concatenation is equal to the argument
-- meaning: xs == ys ++ zs (where xs is an argument)
partitions [h] = [[[], [h]], [[h], []]]
partitions (h:t) = [[], h:t]:[ [h:l,r] | [l,r] <- partitions t]

-- 4.
-- permutations, generates every possible permutation of given argument (list)
permutations :: (Eq a) => [a] -> [[a]]

permutations [] = [[]]
permutations (h:t) = let pp = permutations t in
                     [(take i p) ++ [h] ++ (drop i p) | p <- pp, i <- [0..length p]]

-- 5.
-- nub, delete duplicates from list
nub :: (Eq a) => [a] -> [a]

nub [] = []
nub (h:t) = if h `elem` ret then ret else h:ret
            where ret = nub t

