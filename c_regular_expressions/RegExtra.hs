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
simpl (Empty :> _) = Empty
simpl (_ :> Empty) = Empty
simpl (x :> Eps) = simpl x
simpl (Eps :> x) = simpl x
simpl ((x :> y) :> z) = simpl (x :> (y :> z))
simpl (x :> y) = simpl x :> simpl y

simpl (Empty :| x) = simpl x
simpl (x :| Empty) = simpl x
simpl (Eps :| x) = case nullable x of
                     True -> (simpl x)
                     False -> (Eps :| (simpl x))
simpl (x :| Eps) = simpl (Eps :| x)

simpl (x :| y) = let f = toList (x :| y) [] in
                     listToReg f

simpl (Many x) = case x of
                   Empty -> Eps
                   Eps -> Eps
                   x -> Many (simpl x)
simpl x = x

listNullable :: [Reg c] -> Bool
listNullable [] = False
listNullable (h:t) = if nullable h then True else listNullable t

toList :: Eq c => Reg c -> [Reg c] -> [Reg c]
toList (x :| y) acc = let ac = toList x acc in
                        toList y ac
toList Empty acc = acc
toList Eps acc = if listNullable acc then acc else [Eps] ++ acc
toList x acc = let xs = simpl x in
                 if xs `elem` acc then acc
                 else acc ++ [xs]

listToReg :: [Reg c] -> Reg c
listToReg (h:t) = foldl f h t
        where f :: Reg c -> Reg c -> Reg c
              f r a = r :| a

nullable :: Reg c -> Bool
nullable Eps = True
nullable (Many _) = True
nullable (r1 :> r2) = nullable r1 && nullable r2
nullable (r1 :| r2) = nullable r1 || nullable r2
nullable _ = False

empty :: Eq c => Reg c -> Bool
empty Empty = True
empty (r1 :> r2) = (empty r1) || (empty r2)
empty (r1 :| r2) = (empty r1) && (empty r2)
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
                f r x = simpl $ der x r

accepts :: Eq c => Reg c -> [c] -> Bool
accepts r [] = nullable r
accepts r (h:t) = accepts (der h r) t

mayStart :: Eq c => c -> Reg c -> Bool
mayStart c r = if der c r === Empty then False else True

match :: Eq c => Reg c -> [c] -> Maybe [c]
match r w = Nothing

search :: Eq c => Reg c -> [c] -> Maybe [c]
search r w = Nothing

findall :: Eq c => Reg c -> [c] -> [[c]]
findall r w = []

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

