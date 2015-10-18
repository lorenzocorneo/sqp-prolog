sq_overlap(List) :-
    sq_overlap(List, List).

sq_overlap([X|Xs], List) :-
    sq_overlap(X, List),
    sq_overlap(Xs, List).

sq_overlap(X, [Y|Ys]) :-
    sq_validity(X, Y),
    sq_overlap(X, Ys).

sq_overlap(X, [X|Xs]) :-
    sq_overlap(X, Xs).
sq_overlap(_, []).
sq_overlap([], _).

sq_validity(sq(L1, coord(X1, Y1)), sq(L2, coord(X2, Y2))) :-
    X1 + L1 =< X2;
    X2 + L2 =< X1;
    Y1 + L1 =< Y2;
    Y2 + L2 =< Y1.
