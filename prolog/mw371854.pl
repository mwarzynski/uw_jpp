% Mateusz Warzyński, 371854

use_module(library(lists)).

% Also (np. member/2, append/3, length/2)

% jestWyborem(+AEGraf, -Graf)
% Prawda jeśli Graf jest wyborem AEGraf.
% Gdy dla AEgraf istnieje wiele wyborów, predykat powinien
% odnosić wielokrotnie sukces, przynajmniej raz dla każdego wyboru.
jestWyborem(AEGraf, Graf) :- true.


% jestDFS(+Graf, -Lista)
% Prawda gdy Lista jest lista identyfikatorów
% wierzchołków kolejno odwiedzanych przez algorytm przechodzenia
% grafu Graf w głąb przy przejściu startujacym z pierwszego wierzchołka tego grafu
jestDFS(Graf, Lista) :- true.


% jestADFS(+AEgraf, -Lista)
% Prawda gdy Lista jest lista  ̨ identyfikatorów
% wierzchołków kolejno odwiedzanych przez algorytm przechodzenia w głab przy przejści
% przez pewien graf będacy wyborem z AEgraf. W definicji tego predykatu należy jawnie
% zbudować reprezentacje pewnego wyboru AE-grafu AEgraf.
jestADFS(AEGraf, Lista) :- true.


% jestADFS1(+AEGraf, -Lista)
% Prawda gdy Lista jest lista identyfikatorów
% wierzchołków kolejno odwiedzanych przez algorytm przechodzenia w głab przy przejściu
% przez pewien graf będacy wyborem z AEgraf. W trakcie obliczania tego predykatu
% nie może jawnie być budowana reprezentacja wyboru AE-grafu AEgraf.
jestADFS1(AEGraf, Lista) :- true.

