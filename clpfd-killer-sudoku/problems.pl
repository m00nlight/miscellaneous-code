%% https://en.wikipedia.org/wiki/Killer_sudoku#/media/File:Killersudoku_color.svg
problem(wiki,[[_,_,_,_,_,_,_,_,_],
              [_,_,_,_,_,_,_,_,_],
              [_,_,_,_,_,_,_,_,_],
              [_,_,_,_,_,_,_,_,_],
              [_,_,_,_,_,_,_,_,_],
              [_,_,_,_,_,_,_,_,_],
              [_,_,_,_,_,_,_,_,_],
              [_,_,_,_,_,_,_,_,_],
              [_,_,_,_,_,_,_,_,_]],
        %% board cover
        [[a1, a1, b1, b1, b1, c1, d1, e1, f1],
         [g1, g1, h1, h1, c1, c1, d1, e1, f1],
         [g1, g1, i1, i1, c1, j1, k1, k1, f1],
         [l1, m1, m1, i1, n1, j1, k1, o1, f1],
         [l1, p1, p1, q1, n1, j1, o1, o1, u1],
         [s1, p1, v1, q1, n1, r1, t1, t1, u1],
         [s1, v1, v1, q1, w1, r1, r1, b2, b2],
         [s1, x1, y1, w1, w1, a2, a2, b2, b2],
         [s1, x1, y1, w1, z1, z1, z1, c2, c2]],
        %% sum information
        [a1-3, b1-15, c1-22, d1-4,
         e1-16, f1-15, g1-25, h1-17,
         i1-9, j1-8, k1-20, l1-6,
         m1-14, n1-17, o1-17, p1-13,
         q1-20, r1-20, s1-27, t1-6,
         u1-12, v1-6, w1-10, x1-8,
         y1-16, z1-13, a2-15, b2-14,
         c2-17]).
%% https://www.dailykillersudoku.com/pdfs/17391.pdf
problem(daily_killer_sudoku_hard,
        [[_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_]],
        [[a, a, b, b, c, c, d, d, d],
         [a, e, f, f, c, c, g, h, h],
         [e, e, f, k, c, g, g, i, i],
         [e, j, j, k, l, l, g, i, m],
         [n, o, o, l, l, l, p, p, m],
         [n, q, r, l, l, s, t, t, u],
         [q, q, r, r, v, s, w, u, u],
         [x, x, r, v, v, w, w, u, a1],
         [y, y, y, v, v, z, z, a1,a1]],
        [a-14, b-13, c-22, d-17, e-20,
         f-15, g-15, h-15, i-15, j-8,
         k-14, l-30, m-12, n-13, o-7,
         p-12, q-11, r-19, s-11, t-10,
         u-16, v-30, w-21, x-13, y-12,
         z-7, a1-13]).
%% http://www.sudokufans.org.cn/forums/topic/289/
problem(most_difficult_killer_sudoku,
        [[_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_]],
        [[a, b, c, c, d, e, e, e, f],
         [a, b, b, c, d, d, e, e, f],
         [a, g, h, i, i, i, j, j, f],
         [g, g, h, h, i, k, k, j, j],
         [n, g, h, l, k, k, m, m, m],
         [n, n, l, l, l, k, p, m, m],
         [n, n, o, q, l, p, p, p, s],
         [o, o, o, q, r, r, p, s, s],
         [o, q, q, q, r, r, r, s, s]],
        [a-17, b-15, c-16, d-13, e-18,
         f-24, g-20, h-17, i-24, j-19,
         k-17, l-24, m-26, n-25, o-23,
         p-25, q-30, r-27, s-25]).

%% https://www.dailykillersudoku.com/pdfs/17393.pdf

%% solve the puzzle
%% problem(daily_greater_killer_sudoku_hard, Rows, Splits, Sums),
%% greater_killer_sudoku(Rows, Splits, Sums, [[a,b]-equal, [j, o]-less, [r, v]-equal, [u, y]-greater, [y, v]-equal]),
%% append(Rows, Vs), time(label(Vs)), maplist(portray_clause, Rows).
problem(daily_greater_killer_sudoku_hard,
        [[_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_]],
        [[a, a, b, b, b, c, d, d, d],
         [e, a, b, h, c, c, j, k, k],
         [e, f, g, h, h, i, j, k, k],
         [e, f, g, n, n, i, j, o, o],
         [e, l, m, n, n, n, j, o, p],
         [l, l, m, q, n, n, s, t, p],
         [u, u, m, q, r, r, s, t, p],
         [u, u, m, v, v, r, w, x, p],
         [y, y, y, v, w, w, w, x, x]],
        [d-11, f-9, h-9, k-27, i-8, n-36,
         l-11, m-22, o-14, p-19, q-3,
         u-19, s-17, t-16, w-21, x-10]).