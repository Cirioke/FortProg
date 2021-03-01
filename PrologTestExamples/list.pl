=(X,X).

append([], L, L).
append([E|R], L, [E|RL]) :- append(R, L, RL).

last(L, E) :- append(_, [E], L).

reverse([], []).
reverse([E|R], L) :- reverse(R, UR), append(UR, [E], L).

member(E, [E|_]).
member(E, [_|R]) :- member(E,R).

perm([], []).
perm(L, [E|R]) :- delete(E, L, LwithoutE), perm(LwithoutE, R).

delete(E, L, R) :- append(L1, [E|L2], L), append(L1, L2, R).

sort(L, S) :- perm(L, S), sorted(S).

sorted([]).
sorted([_]).
sorted([E1|[E2|L]]) :- =<(E1, E2), sorted([E2|L]).

length([], 0).
length([_|Xs], N) :- length(Xs, N1), is(N, +(N1, 1)).

lengthP([], o).
lengthP([_|Xs], s(N)) :- lengthP(Xs, N).

maplist(_, [], []).
maplist(P, [X|Xs], [Y|Ys]) :- call(P, X, Y), maplist(P, Xs, Ys).

concatlist([], []).
concatlist([Xs|Xss], Ys) :- concatlist(Xss, Zs), append(Xs, Zs, Ys).

concatmaplist(P, Xs, Ys) :- maplist(P, Xs, Yss), concatlist(Yss, Ys).

% Test query: "append(Xs,Ys,[2,1]), append(Ys,Xs,[1,2]).".
% Expected result: One solution, "{Xs -> [2], Ys -> [1]}".
% Tests whether the mgu found in a resolution step is applied to the whole goal.

% Test query: "append(X,Y,[1,2]).".
% Expected result: Three solutions, "{X -> [], Y -> [1, 2]}",
% "{X -> [1], Y -> [2]}", and "{X -> [1, 2], Y -> []}".

% Test query: "append(_,_,[1,2]).".
% Expected result: Three times the empty substitution.

% Test query: "last([1,2,3],X).".
% Expected result: One solution, "{X -> 3}".

% Test query: "last(Xs,3).".
% Expected result: Infinite solutions, lists with 3 as a last element.

% Test query: "reverse([1,2,3],Xs).".
% Expected result: One solution, "{Xs -> [3, 2, 1]}".

% Test query: "reverse(Xs,[1,2,3]).".
% Expected result: One solution, "{Xs -> [3, 2, 1]}", but non-termination.

% Test query: "reverse(Xs,Xs).".
% Expected result: Infinite solutions, all palindroms.

% Test query: "member(X,[1,2,3]).".
% Expected result: Three solutions, "{X -> 1}", "{X -> 2}", and "{X -> 3}".

% Test query: "member(X,Xs).".
% Expected result: Infinite solutions, where X is within the lists.

% Test query: "delete(X,[1,2,L],Y).".
% Expected result: Three solutions, "{X -> 1, Y -> [2, L]}",
% "{X -> 2, Y -> [1, L]}", and "{X -> L, Y -> [1, 2]}".
% Tests renaming during SLD resolution.

% Test query: "sort([3,1,2],Xs).".
% Expected result: One solution, "{Xs -> [1, 2, 3]}".

% Test query: "append(X,Y,X).".
% Expected result: Infinite solutions, where Y is the empty list.
% Tests strategies and REPL for infinite number of solutions.

% Test query: "length(Xs,2).".
% Expected result: One solution, "{Xs -> [_, _]}", but non-termination.

% Test query: "lengthP(Xs,s(s(o))).".
% Expected result: One solution, "{Xs -> [_, _]}".

% Test query: "maplist(last,[[1,2,3,4],[_,2]],Xs).".
% Expected result: One solution, "{Xs -> [4, 2]}".
% Tests higher-order predicates.

% Test query: "concatlist([[1,2,3],[4]],Xs).".
% Expected result: One solution, "{Xs -> [1, 2, 3, 4]}".
% Tests structures.

% Test query: "concatmaplist(=,[[1,2,3,4],[_,2]],Xs).".
% Expected result: One solution, "{Xs -> [1, 2, 3, 4, Z, 2]}" or some
% equivalent solution ("Z" has to be a fresh variable).
% Tests structures, higher-order predicates and renaming during SLD resolution.

% Test query: "findall(X,member(X,[1,true,[]]),Xs).".
% Expected result: One solution, "{Xs -> [1, true, []]}".
% Tests encapsulation.

% Test query: "findall(true,member(X,[_,_]),Xs).".
% Expected result: One solution, "{Xs -> [true, true]}".
% Tests encapsulation with focus on the template (the first argument) and
% handling of anonymous variables.

% Test query: "findall([X],append(_,_,[_,_]),Xs).".
% Expected result: One solution, "{Xs -> [[C], [B], [A]]}" or some
% equivalent solution ("C", "B", and "A" have to be a fresh variables).
% Tests encapsulation, handling of anonymous variables and generation of fresh
% variables - especially in the context of encapsutilation (this is advanced).

% Test query: "append(_As,Bs,[1,2,3]),append(Cs,Ds,_As).".
% Expected result: Ten solutions, namely all splittings of "[1, 2, 3]" into
% three lists "Bs", "Cs", and "Ds" ("_As" is just a helper variable, but has
% to be part of every solution nonetheless).
% Tests multiple literals in a query and solution printing.
