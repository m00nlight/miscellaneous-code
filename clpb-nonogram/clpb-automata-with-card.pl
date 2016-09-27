:- use_module(library(clpb)).
:- use_module(library(clpfd)).

range(Start, End, Res) :-
    findall(N, between(Start, End, N), Res).

repeat_one(N, Res) :-
    length(Res, N),
    maplist(=(1), Res).

% pair two list until one of the list reach to the end
pair([], _, []) :- !.
pair(_, [], []) :- !.
pair([H1|T1], [H2|T2], [[H1, H2] | TRes]) :-
    pair(T1, T2, TRes).

interpose_tail([], _, []) :- !.
interpose_tail([H|T], Sep, [Sep, H| TRes]) :- interpose_tail(T, Sep, TRes).

interpose([], _, []) :- !.
interpose([X], _, [X]) :- !.
interpose([H|T], Sep, [H|TRes]) :-
    interpose_tail(T, Sep, TRes).

automaton_help(Idx, [Prev, Num], Res) :-
    Idx1 is Idx + 1,
    ( Num =:= 1
    -> ( Prev =:= 0
       -> Res = [arc(s(Idx), 0, s(Idx)), arc(s(Idx), 1, s(Idx1))]
       ;  Res = [arc(s(Idx), 1, s(Idx1)), arc(s(Idx), 0, failure)]
       )
    ;  Res = [arc(s(Idx), 0, s(Idx1)), arc(s(Idx), 1, failure)]
    ).

build_automaton([], [[source(s(1)), sink(s(1))],
		[arc(s(1), 0, s(1)), arc(s(1), 1, failure)]]) :- !.
build_automaton(Cs, [SourceSink, Arcs]) :-
    maplist(repeat_one, Cs, As),
    interpose(As, 0, Bs),
    flatten(Bs, Vs), length(Vs, N), N1 is N + 1,
    range(1, N, Range),
    pair([0|Vs], Vs, Qs),
    maplist(automaton_help, Range, Qs, Ts),
    flatten(Ts, Ps),
    append(Ps, [arc(s(N1), 0, s(N1)),
		arc(s(N1), 1, failure)], Arcs),
    SourceSink = [source(s(1)), sink(s(N1))].


sat_row(Row, [SourceSink, Arcs], RowSum) :-
    sat(card([RowSum], Row)),
    automaton(Row, SourceSink, Arcs),
    labeling(Row).

same_col_length([], _).
same_col_length([H|T], N) :-
    length(H, N),
    same_col_length(T, N).

sat_card(Row, RowSum) :- sat(card([RowSum], Row)).

nonogram(Rows, RowCs, ColCs) :-
    length(RowCs, RowLen), length(Rows, RowLen),
    length(ColCs, ColLen), same_col_length(Rows, ColLen),
    maplist(build_automaton, RowCs, RowAutomaton),
    maplist(build_automaton, ColCs, ColAutomaton),
    maplist(sumlist, RowCs, RowSum),
    maplist(sumlist, ColCs, ColSum),
    transpose(Rows, Cols),
    maplist(sat_row, Rows, RowAutomaton, RowSum),
    maplist(sat_row, Cols, ColAutomaton, ColSum).

test(1, _,
     [[], [3], [1, 1], [3], [1]],
     [[], [], [4], [1, 1], [3]]).

test(2, _,
     [[4], [1, 1], [4], [1], [5], [1]],
     [[], [5], [1, 1, 1], [1, 1, 2], [3, 1], [1]]).

test(3, _,
     [[], [4], [6], [2, 2], [2, 2], [6], [4], [2], [2], [2], []],
     [[], [9], [9], [2, 2], [2, 2], [4], [4], []]).
