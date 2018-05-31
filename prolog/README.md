# Zadanie 3 - Prolog
Zadanie zaliczeniowe z Prologu (8 pkt).

## Treść.

Zdefiniować następujace predykaty:
 - jestWyborem(+AEgraf, -Graf) – odnosi sukces, gdy Graf jest wyborem z AEgraf.
  Gdy dla AEgraf istnieje wiele wyborów, predykat powinien
  odnosić wielokrotnie sukces, przynajmniej raz dla każdego wyboru.
 - jestDFS(+Graf, -Lista) – odnosi sukces, gdy Lista jest lista identyfikatorów
  wierzchołków kolejno odwiedzanych przez algorytm przechodzenia
  grafu Graf w głąb przy przejściu startujacym z pierwszego wierzchołka tego grafu.
 - jestADFS(+AEgraf, -Lista) – odnosi sukces, gdy Lista jest lista  ̨ identyfikatorów
  wierzchołków kolejno odwiedzanych przez algorytm przechodzenia w głab przy przejściu
  przez pewien graf będacy wyborem z AEgraf. W definicji tego predykatu należy jawnie
  zbudować reprezentacje pewnego wyboru AE-grafu AEgraf.
  Można tego dokonać za pomoca predykatu jestWyborem.
 - jestADFS1(+AEgraf, -Lista) – odnosi sukces, gdy Lista jest lista identyfikatorów
  wierzchołków kolejno odwiedzanych przez algorytm przechodzenia w głab przy przejściu
  przez pewien graf będacy wyborem z AEgraf. W trakcie obliczania tego predykatu
  nie może jawnie być budowana reprezentacja wyboru AE-grafu AEgraf.

## Format rozwiązania.

Należy oddać jeden plik o nazwie
    `<inicjały_(dwie_litery)><nr_albumu_(6_cyfr)>.pl`
Plik nie powinien importować żadnych innych plików (por. uwaga poniżej).

Pierwszy wiersz pliku powinien zawierać (krótki) komentarz z imieniem, nazwiskiem i numerem albumu autora programu.

## Zasady.

W rozwiązaniu wolno korzystać wyłącznie:
   - z predykatów/konstrukcji przedstawionych na wykładzie
   - z tzw. wbudowanych predykatów (np. member/2, append/3, length/2)
   - ze standardowej biblioteki SICStus Prologu o nazwie lists
     (ładowanie:     :- use_module(library(lists))).

Nie wolno korzystać:
   - z żadnych innych bibliotek (oprócz biblioteki lists),
   - z (wbudowanych) predykatów nieprzedstawionych na wykładzie.

Programy będą testowane na komputerze students pod SICStus Prologiem.
Programy, które nie będą poprawnie kompilowały się (działały) pod SICStus Prologiem nie uzyskają maksymalnej oceny (choćby poprawnie
kompilowały się i działały pod inną wersją Prologu).

Rozwiązania muszą być całkowicie samodzielne.  W szczególności wszelkie zapożyczenia z internetu oraz prace zbiorowe są niedozwolone.
