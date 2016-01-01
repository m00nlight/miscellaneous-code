:- use_module(library(clpfd)).

% non termination due to infinite solution to constrain of F #= F1 + F2.
n_fib1(0, 1).
n_fib1(1, 1).
n_fib1(N, F) :-
    N #> 1, N1 #= N - 1, N2 #= N - 2,
    F #= F1 + F2,
    n_fib1(N1, F1), n_fib1(N2, F2).

% still non termination after add constrain of F #> 0.
n_fib2(0, 1).
n_fib2(1, 1).
n_fib2(N, F) :-
    N #> 1, N1 #= N - 1, N2 #= N - 2,
    F #= F1 + F2, F #> 0,
    n_fib2(N1, F1), n_fib2(N2, F2).
    

% reorder F #> 0 and F #= F1 + F2 still do not terminate
n_fib3(0, 1).
n_fib3(1, 1).
n_fib3(N, F) :-
    N #> 1, N1 #= N - 1, N2 #= N - 2,
    F #> 0, F #= F1 + F2,
    n_fib3(N1, F1), n_fib3(N2, F2).


% terminate add constrains of F1 #> 0, F2 #> 0.
n_fib4(0, 1).
n_fib4(1, 1).
n_fib4(N, F) :-
    N #> 1, N1 #= N - 1, N2 #= N - 2,
    F #= F1 + F2, F1 #> 0, F2 #> 0,
    n_fib4(N1, F1), n_fib4(N2, F2).


% reorder the clauses will cause non termination again.
n_fib5(0, 1).
n_fib5(1, 1).
n_fib5(N, F) :-
    N #> 1, N1 #= N - 1, N2 #= N - 2,
    n_fib5(N1, F1), n_fib5(N2, F2),
    F #= F1 + F2, F1 #> 0, F2 #> 0.


% accerlate n_fib4
n_fib6(0, 1, 0).
n_fib6(1, 1, 1).
n_fib6(N, F, F1) :-
    N #> 1, N1 #= N - 1, 
    F #= F1 + F2,
    F1 #> 0, F2 #> 0,
    n_fib6(N1, F1, F2).