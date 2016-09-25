:- use_module(library(clpb)).
:- use_module(library(clpfd)).

:- op(100, xf, *).
:- op(100, xf, +).
:- op(100, xfy, **).

regex(C)                --> [C].
regex([T|Ts])           --> regex(T), regex(Ts).
regex([])               --> [].
regex(eps)              --> [].
regex(_*)               --> [].
regex(R*)               --> regex(R), regex(R*).

regex(R+)               --> regex(R), regex(R*).

regex((R1|R2))          --> ( regex(R1) ; regex(R2) ).

regex(range(R,N,M))     -->
    {between(N,M,L),
     length(D,L),
     maplist(copy_term(R),D)
    }, regex(D).

regex(repeat(R, N)) -->
    length(D, N),
    maplist(copy_term(R), D),
    regex(D).

regex(R**N) --> regex(range(R, N, N)).

constraint_2_regex_tail([], [0*]) :- !.
constraint_2_regex_tail([H|T], [0+, 1**H | TRes]) :-
    constraint_2_regex_tail(T, TRes).

constraint_2_regex([], [0*]) :- !.
constraint_2_regex([X], [0*, 1**X, 0*]) :- !.
constraint_2_regex([H|T], [0*, 1**H| RegexTail]) :-
    constraint_2_regex_tail(T, RegexTail).

sat_row(Row, CsRegex) :-
    phrase(regex(CsRegex), Row),
    labeling(Row).

same_col_length([], _).
same_col_length([H|T], N) :-
    length(H, N),
    same_col_length(T, N).

nonogram(Rows, RowCs, ColCs) :-
    length(RowCs, RowLen), length(Rows, RowLen),
    length(ColCs, ColLen), same_col_length(Rows, ColLen),
    maplist(constraint_2_regex, RowCs, RowCsRegex),
    maplist(constraint_2_regex, ColCs, ColCsRegex),
    transpose(Rows, Cols),
    maplist(sat_row, Rows, RowCsRegex),
    maplist(sat_row, Cols, ColCsRegex).

test(1, _,
     [[], [3], [1, 1], [3], [1]],
     [[], [], [4], [1, 1], [3]]).

test(2, _,
     [[4], [1, 1], [4], [1], [5], [1]],
     [[], [5], [1, 1, 1], [1, 1, 2], [3, 1], [1]]).

test(3, _,
     [[], [4], [6], [2, 2], [2, 2], [6], [4], [2], [2], [2], []],
     [[], [9], [9], [2, 2], [2, 2], [4], [4], []]).
