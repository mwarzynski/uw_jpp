% Mateusz Warzyński, 371854

:- use_module(library(lists)).
% Also (np. member/2, append/3, length/2)

% jestWyborem(+AEGraf, -Graf)
% Prawda jeśli Graf jest wyborem AEGraf.
% Gdy dla AEgraf istnieje wiele wyborów, predykat powinien
% odnosić wielokrotnie sukces, przynajmniej raz dla każdego wyboru.
jestWyborem(AEGraf, Graf) :-
    length(AEGraf, L1),
    length(Graf, L2),
    L1 = L2,
    jestWyboremA(AEGraf, Graf),
    jestWyboremE(AEGraf, Graf).

% jestWyboremA(+AEGraf, -Graf)
% Prawda jeśli dla każdego v ∈ Va, i każdego <v, v'> ∈ R zachodzi <v, v'> ∈ S.
jestWyboremA(_, []).
jestWyboremA(AEGraf, [V | Vs]) :-
    jestWyboremAWierzcholek(AEGraf, V),
    jestWyboremA(AEGraf, Vs).

% jestWyboremAWierzcholek(Ws, V)
% Prawda jeśli graf złożony tylko z wierzchołka V spełnia założenia
% dotyczące A-wierzchołków jeśli całkowitym grafem jest Ws.
jestWyboremAWierzcholek(_, V) :- wierzcholekE(V).
jestWyboremAWierzcholek([W | _], V) :-
    wierzcholekA(W),
    wierzcholkiTeSame(W, V),
    jestWyboremASprawdzWierzcholki(W, V).
jestWyboremAWierzcholek([_ | Ws], V) :-
    jestWyboremAWierzcholek(Ws, V).
jestWyboremASprawdzWierzcholki([W, WType | Ws], [V, VType | Vs]) :-
    W = V,
    WType = a, VType = a,
    listaPorownaj(Ws, Vs).

% jestWyboremE(+AEGraf, -Graf)
% Prawda jeśli dla każdego v ∈ Ve istnieje <v, v'> ∈ R dla pewnego v', to istnieje
% dokładnie jedno <v, v''> ∈ R, takie że <v, v''> ∈ S.
jestWyboremE(_, []).
jestWyboremE(AEGraf, [V | Vs]) :-
    jestWyboremEWierzcholek(AEGraf, V),
    jestWyboremE(AEGraf, Vs).

% jestWyboremEWierzcholek(Ws, V)
% Prawda jeśli graf złożony tylko z wierzchołka V spełnia założenia
% dotyczące E-wierzchołków jeśli całkowitym grafem jest Ws.
jestWyboremEWierzcholek(_, V) :- wierzcholekA(V).
jestWyboremEWierzcholek([W | _], V) :-
    wierzcholekE(W),
    wierzcholkiTeSame(W, V),
    jestWyboremESprawdzWierzcholki(W, V).
jestWyboremEWierzcholek([_ | Ws], V) :-
    jestWyboremEWierzcholek(Ws, V).
jestWyboremESprawdzWierzcholki([W, WType | Ws], [V, VType | Vs]) :-
    W = V,
    WType = e, VType = e,
    length(Ws, Wlength),
    Wlength > 0,
    length(Vs, Vlength),
    Vlength > 0,
    listaIloczyn(Ws, Vs, X),
    X = 1.
jestWyboremESprawdzWierzcholki([W, WType | Ws], [V, VType | _]) :-
    W = V,
    WType = e, VType = e,
    length(Ws, Wlength),
    Wlength = 0.


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
% jestADFS(AEGraf, Lista) :- true.


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

