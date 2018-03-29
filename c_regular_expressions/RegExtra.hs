-- C Language Regular Expressions interpreter.
--
-- Author:
-- Mateusz Warzyński (371854) <m.warzynski@students.uw.edu.pl>
--

module RegExtra where
import Mon
import Reg
import Data.List

data AB = A | B deriving (Eq, Ord, Show)

infix 4 ===
class Equiv a where
  (===) :: a -> a -> Bool

instance (Eq c) => Equiv (Reg c) where
  x === y = (simpl x) == (simpl y)

instance Mon (Reg c) where
  m1 = Eps
  x <> y = x :> y

simpl :: Eq c => Reg c -> Reg c
simpl r = let n = ssimpl r in
            if r == n then n
            else simpl n

ssimpl :: Eq c => Reg c -> Reg c
ssimpl (Empty :> _) = Empty
ssimpl (_ :> Empty) = Empty
ssimpl (x :> Eps) = ssimpl x
ssimpl (Eps :> x) = ssimpl x
ssimpl ((x :> y) :> z) = ssimpl (x :> (y :> z))
ssimpl (x :> y) = ssimpl x :> ssimpl y
ssimpl (Empty :| x) = ssimpl x
ssimpl (x :| Empty) = ssimpl x
ssimpl (Eps :| x) = if nullable x then (ssimpl x) else (Eps :| ssimpl x)
ssimpl (x :| Eps) = ssimpl (Eps :| x)
ssimpl (x :| y) = let l = regToList (x :| y) [] in listToReg (nub l)
ssimpl (Many x) = case x of
                   Empty -> Eps
                   Eps -> Eps
                   x -> Many (ssimpl x)
ssimpl x = x

nullableList :: [Reg c] -> Bool
nullableList [] = False
nullableList (h:t) = if nullable h then True else nullableList t

regToList :: Eq c => Reg c -> [Reg c] -> [Reg c]
regToList (x :| y) acc = let ac = regToList x acc in regToList y ac
regToList Empty acc = acc
regToList Eps acc = if nullableList acc then acc else [Eps] ++ acc
regToList x acc = let xs = simpl x in
                    if xs `elem` acc then acc
                    else acc ++ [xs]

listToReg :: [Reg c] -> Reg c
listToReg [] = Eps
listToReg (h:t) = foldl f h t
        where f :: Reg c -> Reg c -> Reg c
              f r a = r :| a

nullable :: Reg c -> Bool
nullable Eps = True
nullable (Many _) = True
nullable (x :> y) = nullable x && nullable y
nullable (x :| y) = nullable x || nullable y
nullable _ = False

empty :: Eq c => Reg c -> Bool
empty Empty = True
empty (x :> y) = (empty x) || (empty y)
empty (x :| y) = (empty x) && (empty y)
empty _ = False

der :: Eq c => c -> Reg c -> Reg c
der c (Lit r) = if c == r then Eps else Empty
der c (x :| y) = (der c x) :| (der c y)
der c (x :> y) = case nullable x of
                   True -> ((der c x) :> y) :| (der c y)
                   False -> (der c x) :> y
der c (Many x) = (der c x) :> Many x
der c _ = Empty

ders :: Eq c => [c] -> Reg c -> Reg c
ders w r = foldl f r w
          where f :: Eq c => Reg c -> c -> Reg c
                f r x = simpl (der x r)

accepts :: Eq c => Reg c -> [c] -> Bool
accepts r w = h (simpl r) w
        where h :: Eq c => Reg c -> [c] -> Bool
              h r [] = nullable r
              h r (c:cs) = accepts (der c r) cs

mayStart :: Eq c => c -> Reg c -> Bool
mayStart c r = not (empty (der c r))

match :: Eq c => Reg c -> [c] -> Maybe [c]
match r [] = if nullable r then Just [] else Nothing
match r word = let s = filter (accepts r) (Data.List.inits word) in
                 if length s == 0 then Nothing
                 else Just (last s)

search :: Eq c => Reg c -> [c] -> Maybe [c]
search r [] = match r []
search r word = case match r word of
                    Just w -> Just word
                    Nothing -> search r (tail word)

findall :: Eq c => Reg c -> [c] -> [[c]]
findall r [] = []
findall r word = case match r word of
                   Nothing -> findall r (drop 1 word)
                   Just w -> let words = findall r (drop (maximum [length w, 1]) word) in
                     [w] ++ words

char :: Char -> Reg Char
char = Lit

string :: [Char] -> Reg Char
string = foldr1 (:>) . map Lit

alts :: [Char] -> Reg Char
alts = foldr1 (:|) . map Lit

letter = alts ['a'..'z'] :| alts ['A'..'Z']
digit = alts ['0'..'9']
number = digit :> Many digit
ident = letter :> Many (letter :| digit)

many1 r = r :> Many r

