% Entry point.
% + N: The number of squares to pack
% - In: List of variables to bind solution coordinates,
%      it's in the form [(X0, Y0), (X1, Y1), (X2, Y2)...]
% + S: The side of the optimal surrounding square,
%      assume that we know it a priori
sqp_with_s(N, In, S):-
    get_time(Time0),
    % Generate possible coordinates and every combination of X and Y
    gen_combinations(S, 1, Res),
    % Pick a set from the possible coordinates
    n_from_m(Res, In),
    % Map raw coordinates to sq/2 ADT
    dyn_var(N, In, [], MyRes),
    reverse(MyRes, Rev),
    % Check if assigned coordinates are between 0 and maximum possible coordinate
    coords_validity(N, Rev),
    % Check if squares overlap each other
    sq_overlap(Rev),
    % Check whether current configuration of coordinates fits optimal surrounding square
    check_sq_fit(Rev, S),
    pretty_printing(Rev, S, Time0).

% Search for size of optimal enclosing square as well
% + N: The number of squares to pack
% - In: List of variables to bind solution coordinates
sqp_no_s(N, In):-
    S = N + N - 1,
    T is S + 1,
    (sqp_with_s(N, In, S) -> true ; sqp_no_s(N, In, T)).

sqp_no_s(N, In, S):-
    T is S + 1,
    (sqp_with_s(N, In, S) -> true ; sqp_no_s(N, In, T)).

% Prints result in a human readable way
% + X: List of squares of type sq/2
% + S: Optimal size for enclosing square
% + Time0: Start time
pretty_printing([sq(N, coord(X, Y)) | Xs], S, Time0) :-
    format('Size: ~d, X: ~d, Y: ~d~n', [N, X, Y]),
    pretty_printing(Xs, S, Time0).

pretty_printing([], S, Time0) :-
    format('Optimal size: ~d~n', [S]),
    get_time(Time1),
    Diff is Time1 - Time0,
    format('Time is seconds: ~f~n', [Diff]).

% Combine Mlist with every variable in Nlist
% + Mlist: available values to combine
% - NList: Variables to bind values
n_from_m(Mlist,[E|Nlist]) :-
	select(E,Mlist,Mrest),
	n_from_m(Mrest,Nlist).
n_from_m(_,[]).

select(X,[X|T],T).
select(X,[Y|T],[Y|R]) :- select(X,T,R).

% Maps raw coordinates to sq/2 ADT
% + N: size of the current square
% + Acc: Accumulator
% - Res: Squares in the form of sq/2
dyn_var(N, [H|T], Acc, Res):-
	(X,Y) = H,
	Res2 = [sq(N, coord(X,Y))|Acc],
	M is N-1,
	dyn_var(M, T, Res2, Res).

dyn_var(0,_,Res, Res).

% Generate possible combinations of coordinates for a square
% If L = 1, we get all possible coordinates
% + S: Side of enclosing square
% + L: Side of square in question
% - Res: Coordinates in the form of [(0,0), (0,1), (1,0),...]
gen_combinations(S, L, Res) :-
    gen_coord(S, L, R),
    % R is [0,1,2,3,...]
    gen_comb(R, R, [], Res).

% Iterate through every coordinate and produce combinations
% + List: X and Y coord, [0,1,2,3,...]
% + X: Same as List but we use it for iteration
% + Acc: Accumulator
% - Res: All combinations [(0,0), (0,1), (1,0),...]
gen_comb(List, [X | Xs], Acc, Res) :-
    combine(X, List, [], Res1),
    merge(Res1, Acc, R),
    gen_comb(List, Xs, R, Res).

gen_comb(_, [], Acc, Acc).

% Mix one value for a dimension with all the others
% + X: Current iteration number, i.e. 1
% + Ys: List of available values, i.e. [0,1,2,3]
% + Acc: Accumulator
% - Res: Combination list, i.e. [(1,0), (1,1), (1,2), (1,3)]
combine(X, [Y | Ys], Acc, Res) :-
    combine(X, Ys, [(X,Y) | Acc], Res).
combine(_X, [], Acc, Acc).

% Merge two lists - not in order!
% + Xs: First list
% + List: Second list
% - Res: Merged list
merge([X | Xs], List, Res) :-
    merge(Xs, [X | List], Res).
merge([], List, List).

% Generate coordinate range for permutation
% [0, 1, 2, ..., S - L]
gen_coord(N, L, R) :-
    T is N - L,
    numlist(0, T, R).

% Check whether the current configuration of squares fits in the enclosing square
% + Xs: Current squares configuration
% + S: Side of, possibly, optimal enclosing square
check_sq_fit([sq(L, coord(X, Y)) | Xs], S) :-
    check_coord_fit(X, L, S),
    check_coord_fit(Y, L, S),
    check_sq_fit(Xs, S).

check_sq_fit([], _).

% Check if coordinate plus length is <= enclosing square
% + C: Square coordinate
% + L: Side of square
% + S: Side of possibly optimal enclosing square
check_coord_fit(C, L, S) :-
    C + L =< S.

% Square ADT.
sq(L, coord(X, Y)):- L > 0, coord(X, Y).

% Coordinate ADT.
coord(X, Y):- X >= 0, Y >= 0.

% Checks the validity of a single coordinate accordingly with the coordinate system.
% A square should not exceed maximum coordinate
% + N: Number of squares
% + coord(X, Y): Coordinates of a square
% + L: Side of square
coord_constraint(N, coord(X, Y), L):- max_size(N, S), X + L < S, Y + L < S.

% Recursive checking of the correctness of all the coordinates.
% + N: Number of squares
% + Xs: configuration of squares
coords_validity(_, []).
coords_validity(N, [sq(L,X)|Xs]):- coord_constraint(N, X, L), coords_validity(N, Xs).

% Checks for overlapping squares.
% It evaluates each square with all other
% + List: All squares configuration
sq_overlap(List):-
	sq_overlap(List, List).

% Helper function for no overlapping check
% + Xs: List of squares to iterate
% + List: List of all squares
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
% + sq(L1, coord(X1, Y1)): Square one
% + sq(L2, coord(X2, Y2)): Square two
sq_validity(sq(L1, coord(X1, Y1)), sq(L2, coord(X2, Y2))):-
	X1 + L1 =< X2;
	X2 + L2 =< X1;
	Y1 + L1 =< Y2;
	Y2 + L2 =< Y1.

% States the maximum coordinate for the system.
% + N: Number of squares
% - S: maximum system coordinate
max_size(N, S):- sigma(1, N, S).

% Implementation of sigma mathematical operator.
% + X: Starting point of sigma
% + Y: Finish point of sigma
% - Z: Result of sigma
sigma(X, X, X).
sigma(X, Y, Z):-
	Y > X,
	N is X + 1,
	sigma(N, Y, T),
	Z is X + T.
