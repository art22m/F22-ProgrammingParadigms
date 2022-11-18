% Murashko Artem SD20-01 %
% Programming Paradigms Fall 2022 %
% Problem Set #11 %

% Exercise 1
% 1.a
minimum([H], H).
minimum([H1, H2|T], X) :-  H1 =< H2, minimum([H1|T], X).
minimum([H1, H2|T], X) :-  H1 > H2, minimum([H2|T], X).

maximum([H], H).
maximum([H1, H2|T], X) :-  H1 >= H2, maximum([H1|T], X).
maximum([H1, H2|T], X) :-  H1 < H2, maximum([H2|T], X).

% 1.b TODO
unifiable(_, []).
unifiable(Term, [H|T]) :- not(not(Term = H)), unifiable(Term, T).

leastSpecificHelper(Term, [Term|_], T2) :- unifiable(Term, T2), !.
leastSpecificHelper(Term, [_|T1], T2) :- leastSpecificHelper(Term, T1, T2).

leastSpecific(Term, L) :- leastSpecificHelper(Term, L, L).

% Exercise 2
% 2.a
remove(_, [], []).
remove(E, [E|T], L):- remove(E, T, L), !.
remove(E, [H|T1], [H|T2]):- remove(E, T1, T2).

% 2.b
removeU(_, [], []).
removeU(Term, [H|T], L):- not(not(Term = H)), removeU(Term, T, L), !.
removeU(Term, [H|T1], [H|T2]):- not(Term = H), removeU(Term, T1, T2).

% Ecercise 3
% X >= 0
nat(0).
nat(N) :- nat(K), N is K+1.

% X <= Y
nat(0, 0) :- !.
nat(0, Max) :- Max > 0.
nat(N, Max) :- M is Max-1, nat(K, M), N is K+1.

% GCD
gcd(0, Y, 1):- not(Y is 1), !, fail.
gcd(X, 0, X):- !.
gcd(0, X, X):- !.
gcd(X, Y, D):- X =< Y, !, Z is Y - X, gcd(X, Z, D).
gcd(X, Y, D):- gcd(Y, X, D).
    
% Two numbers N and M are coprime if there does not exist 
% a pair of numbers 1 ≤ K ≤ M and 1 ≤ L < N such that N×K = M×L
coprime(X, Y) :- nat(X), nat(Y, X), not(Y is 1), not(Y is 0), gcd(Y, X, 1).