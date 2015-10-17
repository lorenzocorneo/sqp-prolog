n_from_m(Mlist,[E|Nlist]) :-
	select(E,Mlist,Mrest),
	n_from_m(Mrest,Nlist).
n_from_m(_,[]).

select(X,[X|T],T).
select(X,[Y|T],[Y|R]) :- select(X,T,R).


% dynamic var
dyn_var(N, [H|T], Acc, Res):-
	(X,Y) = H,
	Res2 = [sq(N, coord(X,Y))|Acc],
	M is N-1,
	dyn_var(M, T, Res2, Res).

dyn_var(0,_,Res, Res).	


% The top of the iceberg.
sqp(N, In, S):-
    gen_combinations(S, 1, Res),
    n_from_m(Res,In),
    dyn_var(N,In,[],MyRes),
    reverse(MyRes, Rev),
    coords_validity(N, Rev),
    sq_overlap(Rev),
    check_sq_fit(Rev, S).

% Generate possible combinations of coordinates for a square
% If L = 1, we get all possible coordinates
gen_combinations(N, L, Res) :-
    gen_coord(N, L, R),
    gen_comb(R, R, [], Res).

% Iterate through every coordinate and produce combinations
gen_comb(List, [X | Xs], Acc, Res) :-
    combine(X, List, [], Res1),
    merge(Res1, Acc, R),
    gen_comb(List, Xs, R, Res).

gen_comb(_, [], Acc, Acc).

% Combine one possible coordinate with all the others
combine(X, [Y | Ys], Acc, Res) :-
    combine(X, Ys, [(X,Y) | Acc], Res).
combine(_X, [], Acc, Acc).

% Merge two lists - not in order!
merge([X | Xs], List, Res) :-
    merge(Xs, [X | List], Res).
merge([], List, List).

% Generate coordinate range for permutation
gen_coord(N, L, R) :-
    T is N - L,
    numlist(0, T, R).

% Check whether the current configuration of squares fit in the enclosing square
check_sq_fit([sq(L, coord(X, Y)) | Xs], S) :-
    check_coord_fit(X, L, S),
    check_coord_fit(Y, L, S),
    check_sq_fit(Xs, S).

check_sq_fit([], _).

% Check if coordinate plus length is <= enclosing square
check_coord_fit(C, L, S) :-
    C + L =< S.

% Square ADT.
sq(L, coord(X, Y)):- L > 0, coord(X, Y).

% Coordinate ADT.
coord(X, Y):- X >= 0, Y >= 0.

% Checks the validity of a single coordinate accordingly with the coordinate system.
coord_constraint(N, coord(X, Y), L):- max_size(N, S), X + L < S, Y + L < S.

% Recursive checking of the correctness of all the coordinates.
coords_validity(_, []).
coords_validity(N, [sq(L,X)|Xs]):- coord_constraint(N, X, L), coords_validity(N, Xs).

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
sq_overlap([], _).

% Goals definition for overlap checking.
sq_validity(sq(L1, coord(X1, Y1)), sq(L2, coord(X2, Y2))):-
	X1+L1=<X2;
	X2+L2=<X1;
	Y1+L1=<Y2;
	Y2+L2=<Y1.

% States the maximum coordinate for the system.
max_size(N, S):- sigma(1, N, S).

% Implementation of sigma mathematical operator.
sigma(X, X, X).
sigma(X, Y, Z):-
	Y>X,
	N is X+1,
	sigma(N, Y, T),
	Z is X+T.
