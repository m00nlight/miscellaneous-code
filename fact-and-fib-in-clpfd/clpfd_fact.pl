:- use_module(library(clpfd)).

% correct way to do factorial in clpfd
n_factorial(0, 1).
n_factorial(N, F) :-
    N #> 0, N1 #= N - 1, F #= N * F1,
    n_factorial(N1, F1).


% non termination after reordering.
n_factorial2(0, 1).
n_factorial2(N, F) :-
    N #> 0, N1 #= N - 1,
    n_factorial2(N1, F1),
    F #= N * F1.