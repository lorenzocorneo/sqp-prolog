% The top of the iceberg.
sqp(N, Xs, _S):- coords_validity(N, Xs).

% Square ADT.
sq(L, coord(X, Y)):- L > 0, coord(X, Y).

% Coordinate ADT.
coord(X, Y):- X >= 0, Y >= 0.

% Checks the validity of a single coordinate accordingly with the coordinate system.
coord_constraint(N, coord(X, Y)):- max_size(N, S), X < S, Y < S.

% Recursive checking of the correctness of all the coordinates.
coords_validity(_, []).
coords_validity(N, [sq(_,X)|Xs]):- coord_constraint(N, X), coords_validity(N, Xs).

% Checks for overlapping squares.
sq_overlap(List):-
	sq_overlap(List, List).

sq_overlap([X|Xs], List):-
	sq_overlap(X, List),
	sq_overlap(Xs, List).

sq_overlap(X, [Y|Ys]):-
	sq_validity(X, Y),
	sq_overlap(X, Ys).

sq_overlap(X, [X|Xs]):- sq_overlap(X, Xs).
sq_overlap(_, []).

% Goals definition for overlap checking.
sq_validity(sq(L1, coord(X1, Y1)), sq(L2, coord(X2, Y2))):-
	(X1+L1=<X2;
	X2+L2=<X1),
	(Y1+L1=<Y2;
	Y2+L2>=Y1).

% States the maximum coordinate for the system.
max_size(N, S):- sigma(1, N, S).

% Implementation of sigma mathematical operator.
sigma(X, X, X).
sigma(X, Y, Z):-
	Y>X,
	N is X+1,
	sigma(N, Y, T),
	Z is X+T.