:- use_module(library(clpfd)).
:- ensure_loaded('problems.pl').

get_value_by_key([], _, _) :- !.
get_value_by_key([X-Y|_], X, Y) :- !.
get_value_by_key([Z-_|Ps], X, Y) :- Z \= X, get_value_by_key(Ps, X, Y).

region_constrain(Sums, X-Vs) :-
    get_value_by_key(Sums, X, Value),
    all_distinct(Vs),
    sum(Vs, #=, Value).

fourtyfive_relation(Vs) :- sum(Vs, #=, 45).

killer_sudoku(Rows, Splits, Sums) :-
    length(Rows, 9), maplist(same_length(Rows), Rows),
    length(Splits, 9), maplist(same_length(Splits), Splits),
    append(Rows, Vs), Vs ins 1..9,
    append(Splits, Ks),
    pairs_keys_values(Pairs, Ks, Vs),
    sort(1, @>=, Pairs, SortedPairs),
    group_pairs_by_key(SortedPairs, Regions),
    maplist(all_distinct, Rows),
    maplist(fourtyfive_relation, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
    maplist(fourtyfive_relation, Columns),
    Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
    blocks(As, Bs, Cs),blocks(Ds, Es, Fs),blocks(Gs, Hs, Is),
    maplist(region_constrain(Sums), Regions).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
    all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
    N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8 + N9 #= 45, 
    blocks(Ns1, Ns2, Ns3).


compare_constrain(Regions, [X, Y]-Relation) :-
    get_value_by_key(Regions, X, Xs),
    get_value_by_key(Regions, Y, Ys),
    all_distinct(Xs), all_distinct(Ys),
    sum(Xs, #=, Vx),
    sum(Ys, #=, Vy),
    (Relation = less
    ->  Vx #< Vy
    ;   ( Relation = equal
        -> Vx #= Vy
        ;  Vx #> Vy
        )
    ).

has_sum_info(Pos, X-_) :- member(X, Pos).

greater_killer_sudoku(Rows, Splits, Sums, Compares) :-
    length(Rows, 9), maplist(same_length(Rows), Rows),
    length(Splits, 9), maplist(same_length(Splits), Splits),
    append(Rows, Vs), Vs ins 1..9,
    append(Splits, Ks),
    pairs_keys_values(Pairs, Ks, Vs),
    sort(1, @>=, Pairs, SortedPairs),
    group_pairs_by_key(SortedPairs, Regions),
    maplist(all_distinct, Rows),
    maplist(fourtyfive_relation, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
    maplist(fourtyfive_relation, Columns),
    Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
    blocks(As, Bs, Cs),blocks(Ds, Es, Fs),blocks(Gs, Hs, Is),
    maplist(compare_constrain(Regions), Compares),
    pairs_keys(Sums, PosHasSumValue),
    pairs_values(Regions, Vss),
    maplist(all_distinct, Vss),
    include(has_sum_info(PosHasSumValue), Regions, RegionsHasSum),
    maplist(region_constrain(Sums), RegionsHasSum).
