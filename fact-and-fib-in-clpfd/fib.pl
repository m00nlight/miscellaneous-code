n_fib1(0, 1).
n_fib1(1, 1).
n_fib1(N, F) :-
    N > 1, N1 is N - 1, N2 is N - 2,
    n_fib1(N1, F1),
    n_fib1(N2, F2),
    F is F1 + F2.


n_fib2(0, 0, 1).
n_fib2(1, 1, 1).
n_fib2(N, F, F1) :-
    N > 1, N1 is N - 1, n_fib2(N1, F1, F2),
    F is F1 + F2.
