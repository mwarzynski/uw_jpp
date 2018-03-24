# Projekt 1 - Haskell

## Wstęp

Dany (w pliku Reg.hs) typ danych reprezentujący wyrażenia regularne nad alfabetem c:

```Haskell
data Reg c
  = Lit c           -- one character
  | Reg c :> Reg c  -- concatenation
  | Reg c :| Reg c  -- alternative (sum)
  | Many (Reg c)    -- star
  | Eps             -- empty word
  | Empty           -- empty language
  deriving (Eq,Show)
```

Dla danego wyrażenia `r`, przez `L(r)` oznaczamy język tego wyrażenia, rozumiany w standardowy sposób (w razie wątpliwości prosze pytać). Mówimy, że `r` akceptuje słowo w gdy `w` należy do `L(r)`. Podobnie mówimy, że wyrażenie akceptuje (albo reprezentuje) język. Wyrażenia regularne `r1` i `r2` nazywamy równoważnymi jeśli akceptują one ten sam język.

Uzupełnij moduł `RegExtra` (którego szkielet dany jest w pliku RegExtra0.hs) o definicje omówione poniżej (ewentualnie zastepując występujące w nim zaślepki).


## Equiv

Zdefiniuj instancje klasy `Equiv` dla `Reg`:

```Haskell 
infix 4 ===
class Equiv a where
  (===) :: a -> a -> Bool

instance (Eq c) => Equiv (Reg c) where
```

tak aby relacja `===` była relacją zwrotną, symetryczną i przechodnią
a ponadto spełniony był warunek:

```Haskell
equivCompatible c d = (Lit c) === (Lit d) ==> c == d
```

Intencją relacji `(===)` jest równoważność (tu wyrażeń regularnych).
Wystarczy jednak zdefiniować relację mniej dokładną, byle tylko spełnione
były warunki podane w pliku TestReg.hs. Można nawet zacząć od przyjęcia

```Haskell
(===) = (==)
```

a potem doszlifować ją tak, by spełniała podane warunki.


## Monoid
Dana (Mon.hs) klasa reprezentującą monoidy

```Haskell
class Mon m where
  m1 :: m
  (<>) :: m -> m -> m
```

Uzupełnij instancję instance `Mon (Reg c)` tak, aby dla dowolnych `x y z`
spełnione były własności:

```Haskell
leftUnit x = m1 <> x === x
rightUnit x =  x <> m1 === x
assoc x y z = (x<>y)<>z === x<>(y<>z)
```

(mówimy że np. własność `assoc` jest spełniona jeśli dla każdych `x y z` 
odpowiedniego typu `assoc x y z` daje wartość `True`)

**Uwaga**: udostępniamy program TestReg.hs, który po skompilowaniu i uruchomieniu
testuje wymagane własności. Wymaga on zainstalowania pakietu `QuickCheck`.


## Słowo puste i język pusty

Napisz funkcję:
```Haskell
nullable, empty :: Reg c -> Bool
```

taką, że:
 - `nullable r == True` gdy słowo puste należy do języka
 - `empty r == True` gdy język jest pusty

### Testy:
```Haskell
nullableUnit = nullable m1
nullableOp x y = nullable x && nullable y ==> nullable (x <> y)
```


## Upraszczanie

Dla danego wyrażenia regularnego nierzadko możemy podać prostsze wyrażenie równoważne,
np `Eps :> (Lit 0 :| Empty)` jest równoważne `Lit 0`.

Napisz funkcję:

```Haskell
simpl :: Eq c => Reg c -> Reg c
```

dającą prostsze wyrażenie równoważne argumentowi `a` (w jakimś sensie).
Niektóre potrzebne uproszczenia ujawnią się w późniejszych etapach.

### Testy:

```Haskell
nullableSimpl x = nullable x `iff` nullable (simpl x)
emptySimpl x = empty x `iff` empty (simpl x)
```


## Pochodne

Pochodną języka `L` względem `c` jest język zawierający słowa `w` takie, że `cw`
należy do `L`. Pochodna języka regularnego jest zawsze językiem regularnym.

Napisz funkcje:

```Haskell
der :: Eq c => c -> Reg c -> Reg c
ders :: Eq c => [c] -> Reg c -> Reg c
```

dające pochodną wyrażenia regularnego względem (odpowiednio) jednego znaku i ciągu znaków.

**Uwaga**: nie od rzeczy może być tu wykorzystanie funkcji `simpl`.
Jaki rozmiar ma `ders (replicate 1000 A) (Many (Lit A) :> Lit B)`?


## Dopasowania

Łatwo zauważyć, że słowo należy do języka wtw gdy pochodna języka względem tego słowa
zawiera słowo puste. Wykorzystując ten fakt i funkcje opisane powyżej, napisz funkcje:
```Haskell
accepts :: Eq c => Reg c -> [c] -> Bool
mayStart :: Eq c => c -> Reg c -> Bool
match :: Eq c => Reg c -> [c] -> Maybe [c]
search :: Eq c => Reg c -> [c] -> Maybe [c]
findall :: Eq c => Reg c -> [c] -> [[c]]
```

takie, że:
 - `accepts r` w daje `True` gdy w należy do `L(r)`.
 - `mayStart c r` daje `True` gdy `L(r)` zawiera słowo zaczynające się od `c`
 - `match r w` daje `Just p`, gdzie `p` to najdłuższy prefiks w należacy do `L(r)`, `Nothing` gdy nie ma takiego.
 - `search r w` daje `Just u` gdzie `u` to pierwsze (najdłuższe) podsłowo `w` akceptowane przez `r`, `Nothing` gdy nie ma takiego.
 - `findall r w` daje listę wszystkich (lokalnie najdłuższych) podsłów w pasujących do `r`.

