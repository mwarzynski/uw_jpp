% Mateusz Warzyński, 371854

:- use_module(library(lists)).
% Also (np. member/2, append/3, length/2)

% jestWyborem(+AEGraf, -Graf)
% Prawda jeśli Graf jest wyborem AEGraf.
% Gdy dla AEgraf istnieje wiele wyborów, predykat powinien
% odnosić wielokrotnie sukces, przynajmniej raz dla każdego wyboru.
jestWyborem([], []).
jestWyborem([A|As], [G|Gs]) :-
    wierzcholekID(A, Aid),
    wierzcholekID(G, Gid),
    Aid = Gid,
    wierzcholekSasiedzi(A, Asasiedzi),
    wierzcholekSasiedzi(G, Gsasiedzi),
    wierzcholekTyp(A, Atyp),
    wierzcholekTyp(G, Gtyp),
    Atyp = Gtyp,
    jestWyboremWierzcholki(Gtyp, Asasiedzi, Gsasiedzi),
    jestWyborem(As, Gs).

jestWyboremWierzcholki(e, [], []).
jestWyboremWierzcholki(e, As, [G]) :-
    member(G, As).
jestWyboremWierzcholki(a, As, Gs) :-
    listaPermutuj(As, Gs).

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
    jestWyborem(AEGraf, W),
    jestDFS(W, Lista).

% jestADFS1(+AEGraf, -Lista)
% Prawda gdy Lista jest lista identyfikatorów
% wierzchołków kolejno odwiedzanych przez algorytm przechodzenia w głab przy przejściu
% przez pewien graf będacy wyborem z AEgraf. W trakcie obliczania tego predykatu
% nie może jawnie być budowana reprezentacja wyboru AE-grafu AEgraf.
% jestADFS1(AEGraf, Lista) :-

% wierzcholekOID(Vs, Id, Wynik)
% Ustawia na Wynik wierzchołek o podanym Id z listy wierzchołków Vs.
wierzcholekOID([V | _], Id, R) :-
    wierzcholekID(V, VId),
    VId = Id,
    R = V.
wierzcholekOID([_| Vs], Id, R) :- wierzcholekOID(Vs, Id, R).

wierzcholekID([V | _], Id) :- Id = V.
wierzcholekTyp([_, Typ | _], T) :- T = Typ.
wierzcholekSasiedzi([_, _ | Vs], Sasiedzi) :- Sasiedzi = Vs.

listaDoGraf([], _, Graf) :- Graf = [].
listaDoGraf([H|T], Lista, Graf) :-
    wierzcholekID(H, HId),
    not(member(HId, Lista)),
    listaDoGraf(T, Lista, Graf).
listaDoGraf([H|T], Lista, Graf) :-
    wierzcholekID(H, HId),
    member(HId, Lista),
    listaDoGraf(T, Lista, Gg),
    wierzcholekTyp(H, Htyp),
    wierzcholekSasiedzi(H, Hs),
    wierzcholekPrzytnij(HId, Htyp, Hs, NV),
    append([NV], Gg, Graf).

grafDoLista([], Lista) :- Lista = [].
grafDoLista([G|Gs], Lista) :-
    grafDoLista(Gs, L),
    wierzcholekID(G, GId),
    append([GId], L, Lista).

wierzcholekPrzytnij(VId, a, Vs, V) :-
    append([VId, a], Vs, V).
wierzcholekPrzytnij(VId, e, [], V) :-
    V = [VId, e].
wierzcholekPrzytnij(VId, e, [Vs | _], V) :-
    append([VId, e], Vs, V).

% listaPorownaj(A, B)
% Prawda, jeśli wszystkie elementy listy A są również w liście B.
listaPorownaj([], _).
listaPorownaj([A|As], B) :-
    member(A, B),
    listaPorownaj(As, B).

listaOdwroc([], []).
listaOdwroc([H|T],Z) :-
    listaOdwroc(T,Z1),
    append(Z1, [H], Z).

listaPermutuj([],[]).
listaPermutuj([X|Y],Z) :-
    listaPermutuj(Y,W),
    listaZamien(X,Z,W).

listaZamien(X, [X|R], R).
listaZamien(X, [F|R], [F|S]) :-
    listaZamien(X,R,S).

