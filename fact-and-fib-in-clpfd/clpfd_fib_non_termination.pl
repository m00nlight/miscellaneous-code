:- use_module(library(clpfd)).

n_fib1(0, 1).
n_fib1(1, 1).
n_fib1(N, F) :-
    N #> 1, N1 #= N - 1, N2 #= N - 2,
    n_fib1(N1, F1), n_fib1(N2, F2),
    F #= F1 + F2.
