-- C Language Regular Expressions interpreter.
--
-- Author:
-- Mateusz Warzyński (371854) <m.warzynski@students.uw.edu.pl>
--

-- Rozwiązania mają być samodzielne. Wszelkie zapożyczenia z internetu itp.
--    należy wyrażnie zaznaczyć.
-- Należy oddać wyłącznie plik RegExtra.hs. Nie może on importować nic ponadto
--    co jest już importowane w RegExtra0.hs
-- Rozwiązania będą oceniane pod kątem:
--    - spełnienia warunków zadania; rozwiązania nie przechodzące testów będą nisko oceniane,
--          nawet na 0p; rozwiązania będą też poddawane dodatkowym testom.
--    - właściwego wykorzystania mechanizmów paradygmatu funkcyjnego i języka Haskell,
--          tudzież czytelności i stylu.
--    - rozwiązania skrajnie nieefektywne będą karane; przy porządnym rozwiązaniu testy
--          przechodzą w ok 1s. Rozwiązanie, gdzie będzie to trwało ponad 2 minuty
--          uznamy za nieefektywne.

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

simpl (Empty :> r) = Empty
simpl (r :> Empty) = Empty
simpl (r :> Eps) = simpl r
simpl (Eps :> r) = simpl r

simpl (x :> (y :> z)) = simpl x :> simpl y :> simpl z
simpl ((x :> y) :> z) = simpl (x :> (y :> z))
simpl (x :> y) = simpl x :> simpl y

simpl (Empty :| r) = simpl r
simpl (r :| Empty) = simpl r

simpl (Eps :| r) = case nullable r of
                     True -> (simpl r)
                     False -> (Eps :| (simpl r))
simpl (r :| Eps) = simpl (Eps :| r)

simpl (x :| (y :| z)) = if x === z then
                          simpl (x :| y)
                        else if x === y then
                          simpl (x :| z)
                        else
                          simpl x :| simpl y :| simpl z
simpl ((x :| y) :| z) = simpl (x :| (y :| z))
simpl (x :| y) = if x === y then
                   simpl x
                 else
                   simpl x :| simpl y

simpl (Many r) = case r of
                   Empty -> Eps
                   Eps -> Eps
                   r -> Many (simpl r)

simpl r = r

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
empty r = False

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

