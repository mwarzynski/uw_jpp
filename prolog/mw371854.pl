% Mateusz Warzyński, 371854

:- use_module(library(lists)).
% Also (np. member/2, append/3, length/2)

% jestWyborem(+AEGraf, -Graf)
% Prawda jeśli Graf jest wyborem AEGraf.
% Gdy dla AEgraf istnieje wiele wyborów, predykat powinien
% odnosić wielokrotnie sukces, przynajmniej raz dla każdego wyboru.
jestWyborem(AEGraf, Graf) :-
    jestWyboremA(AEGraf, Graf),
    jestWyboremE(AEGraf, Graf).


% jestDFS(+Graf, -Lista)
% Prawda gdy Lista jest listą identyfikatorów
% wierzchołków kolejno odwiedzanych przez algorytm przechodzenia
% grafu Graf w głąb przy przejściu startujacym z pierwszego wierzchołka tego grafu
jestDFS(_, []) :- false.
jestDFS([G | Gs], Lista) :-
    wierzcholekID(G, GId),
    jestDFS2([G | Gs], [GId], [], Lista).

% jestDFS2(Graf, ListaPrzejscia, Odwiedzone, Wynik)
% Predykat pomocniczy dla jestDFS.
jestDFS2(_, [], Odwiedzone, Wynik) :-
    listaOdwroc(Odwiedzone, Wynik).
jestDFS2(Graf, [Id | Stos], Odwiedzone, Wynik) :-
    member(Id, Odwiedzone),
    jestDFS2(Graf, Stos, Odwiedzone, Wynik).
jestDFS2(Graf, [Id | Stos], Odwiedzone, Wynik) :-
    not(member(Id, Odwiedzone)),
    wierzcholekOID(Graf, Id, V),
    wierzcholekSasiedzi(V, S),
    listaPermutuj(S, NS),
    append(NS, Stos, NowyStos),
    jestDFS2(Graf, NowyStos, [Id | Odwiedzone], Wynik).

% jestADFS(+AEgraf, -Lista)
% Prawda gdy Lista jest listą identyfikatorów
% wierzchołków kolejno odwiedzanych przez algorytm przechodzenia w głab przy przejści
% przez pewien graf będacy wyborem z AEgraf. W definicji tego predykatu należy jawnie
% zbudować reprezentacje pewnego wyboru AE-grafu AEgraf.
jestADFS(AEGraf, Lista) :-
    jestDFS(AEGraf, Lista),
    listaDoGraf(AEGraf, Lista, Wybor),
    jestWyborem(AEGraf, Wybor).


% jestADFS1(+AEGraf, -Lista)
% Prawda gdy Lista jest lista identyfikatorów
% wierzchołków kolejno odwiedzanych przez algorytm przechodzenia w głab przy przejściu
% przez pewien graf będacy wyborem z AEgraf. W trakcie obliczania tego predykatu
% nie może jawnie być budowana reprezentacja wyboru AE-grafu AEgraf.
% jestADFS1(AEGraf, Lista) :- true.


wierzcholkiIstnieja(G, [Id, Ids]) :-
    wierzcholekOID(G, Id, _),
    wierzcholkiIstnieja(G, Ids).

% wierzcholekOID(Vs, Id, Wynik)
% Ustawia na Wynik wierzchołek o podanym Id z listy wierzchołków Vs.
wierzcholekOID([V | _], Id, R) :-
    wierzcholekID(V, VId),
    VId = Id,
    R = V.
wierzcholekOID([_| Vs], Id, R) :- wierzcholekOID(Vs, Id, R).

% wierzcholkiTeSame(V, W)
% Prawda, jeśli obydwa wierzchołki są takie same.
% Predykat nie sprawdza sąsiadów.
wierzcholkiTeSame([V, Vt | _], [W, Wt | _]) :-
    V  = W,
    Vt = Wt.

wierzcholekID([V | _], Id) :- Id is V.
wierzcholekSasiedzi([_, _ | Vs], Sasiedzi) :- Sasiedzi = Vs.

% wierzcholekA(V)
% Prawda jeśli przekazany wierzchołek jest typu A.
wierzcholekA([_, Typ | _]) :- Typ = a.

% wierzcholekE(V)
% Prawda jeśli przekazany wierzchołek jest typu A.
wierzcholekE([_, Typ | _]) :- Typ = e.

listaDoGraf(_, [], Graf) :- Graf = [].
listaDoGraf(AEGraf, [H|T], Graf) :-
    wierzcholekOID(AEGraf, H, V),
    listaDoGraf(AEGraf, T, Gg),
    append(Gg, [V], G),
    Graf = G.

% listaPorownaj(A, B)
% Prawda, jeśli wszystkie elementy listy A są również w liście B.
listaPorownaj([], _).
listaPorownaj([A|As], B) :-
    member(A, B),
    listaPorownaj(As, B).

listaIloczyn([], _, X) :- X is 0.
listaIloczyn(_, [], X) :- X is 0.
listaIloczyn([A|As], B, X) :-
   listaIloczyn(As, B, Xx),
   ( member(A, B) -> X is Xx + 1 ; X is Xx ).

listaOdwroc([], []).
listaOdwroc([H|T],Z) :-
    listaOdwroc(T,Z1),
    append(Z1, [H], Z).

listaPermutuj([X|Y],Z) :- listaPermutuj(Y,W), listaZamien(X,Z,W).
listaPermutuj([],[]).

listaZamien(X, [X|R], R).
listaZamien(X, [F|R], [F|S]) :- listaZamien(X,R,S).

