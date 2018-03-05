-- Haskell laboratory 1. --

-- i) write a function permutations, which takes as an argument a list, and returns the list of its permutations
-- permutations [1,2,3] = [[1,2,3],[1,3,2],[2,1,3],[3,1,2],[2,3,1],[3,2,1]]
permutations :: (Eq a) => [a] -> [[a]]
permutations [] = [[]]
permutations (h:t) = let pp = permutations t in
                     [(take i p) ++ [h] ++ (drop i p) | p <- pp, i <- [0..length p]]

-- ii) write a function nub, which takes as an argument a list and returns the same list without the repetitions (the elements shall appear in the same order yet)
nub :: (Eq a) => [a] -> [a]

nub [] = []
nub (h:t) = if h `elem` ret then ret else h:ret
            where ret = nub t

-- iii) write a function fibo, which takes as an argument an integer n, and returns the nth fibonacci number? what is fibo 10? fibo 100?
fiboh a b n = if n > 0 then fiboh b (a+b) (n-1) else b
fibo n = fiboh 0 1 n
-- fibo 100 = 573147844013817084101

-- iv) write a function packing, which packs consecutive equal elements in a list into sublists
packing :: (Eq a) => [a] -> [[a]]

packingh [] l n r = r ++ [replicate n l]
packingh (h:t) l n r = if h == l then
                            packingh t h (n+1) r
                        else
                            packingh t h 1 (r ++ [replicate n l])

packing [] = []
packing (h:t) = packingh t h 1 []
-- packing ["a","a","a","a","b","c","c","a","a","d","d","e","e","e","e"] = [["a","a","a","a"],["b"],["c","c"],["a","a"],["d","d"],["e","e","e","e"]]


-- v) write a function positions which takes as an argument a character a, a string s, and returns the list of integers n such that the nth indexes of s is a

positionsh [] s n ret = ret 
positionsh (h:t) s n ret = if h == s then
                               positionsh t s (n+1) (ret ++ [n])
                           else
                               positionsh t s (n+1) ret

positions s (h:t) = positionsh (h:t) s 0 []
-- positions 'e' "Dlaczego jest tak zimno w Polsce?" = [5,10,31]


-- vi) write a function indexOf which takes as an argument a character c, a string s, and returns a Maybe Int: Nothing if c doesn't appear in s, and Just n if n is such that the nth index of s is c
indexOf :: Char -> String -> Maybe Int
indexOf c s = indexOfH c s 0
    where indexOfH c [] n = Nothing
          indexOfH c (h:t) n = if c == h then (Just n) else indexOfH c t (n+1)
-- indexOf 'a' "Czy ma Ana kota?" = Just 5 (or Just 9, or Just 14)
-- indexOf 'e' "Czy ma Ana kota?" = Nothing


-- vi) write a function triads which takes as an argument a integer n and returns the list of triples of integers x, y, z <= n such that x^2 + y^2 = z^2 (x, y and z shall have no commun divisors)
triads :: Int -> [(Int, Int, Int)]
triads n = [(x, y, z) |
                x <- [1..n],
                y <- [1..n],
                z <- [1..n],
                z <= n,
                x*x + y*y == z*z,
                x <= y,
                y <= z,
                gcd x y == 1,
                gcd y z == 1,
                gcd x z == 1]
-- triads 20 = [(3,4,5),(5,12,13),(8,15,17)]


-- vii) write a function phi which returns the image of an integer n by euleur's function (the number of integer m <= n coprime with n)
phi :: Int -> Int
phi n = length [x | x <- [1..n], gcd x n == 1]
-- phi 10 = 4
