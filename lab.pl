polishFlag(L, H) :- polishFlag(L, [], [], H).

polishFlag([], L, H, G) :- append(L, H, G).
polishFlag(['b'| L], AccB, AccC, H) :- polishFlag(L, ['b' | AccB], AccC, H). 
polishFlag(['c'| L], AccB, AccC, H) :- polishFlag(L, AccB, ['c' | AccC], H).

flatten(L, H) :- flatten(L, [], H).

flatten([], H, H).
flatten([[] | L], Acc, H) :- flatten(L, Acc, H).
flatten([X | L], Acc, H) :- integer(X), flatten(L, [X | Acc], H).
flatten([X | L], Acc, H) :- flatten(X, Acc, J), append(J, Acc, New), flatten(L, New, H).


min([], 0). 
min([H | L], X) :- min(L, H, X).

min([], X, X).
min([E | L], Acc, X) :- E < Acc, min(L, E, X), !.
min([E | L], Acc, X) :- E >= Acc, min(L, Acc, X).