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
simpl ((x :| y) :| z) = simpl (x :| (y :| z))
simpl ((Lit x) :| (Lit y)) = if x == y then
                               Lit x
                             else
                               Lit x :| Lit y
simpl (x :| y) = simpl x :| simpl y
simpl (Many x) = case x of
                   Empty -> Eps
                   Eps -> Eps
                   x -> Many (simpl x)
simpl x = x

nullable :: Reg c -> Bool
nullable Empty = False
nullable Eps = True
nullable (Lit c) = False
nullable (Many r) = True
nullable (r1 :> r2) = nullable r1 && nullable r2
nullable (r1 :| r2) = nullable r1 || nullable r2

empty :: Eq c => Reg c -> Bool 
empty Empty = True
empty (r1 :> r2) = (empty r1) || (empty r2)
empty (r1 :| r2) = (empty r1) && (empty r2)
empty _ = False

der :: Eq c => c -> Reg c -> Reg c
der c r = (Lit c :> r)

ders :: Eq c => [c] -> Reg c -> Reg c
ders [] r = r
ders (h:t) r = (Lit h) :> ders t r

accepts :: Eq c => Reg c -> [c] -> Bool
accepts r w = False

mayStart :: Eq c => c -> Reg c -> Bool
mayStart c r = False

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

