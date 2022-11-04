% Murashko Artem SD20-01 %
% Programming Paradigms Fall 2022 %
% Problem Set #10 %

% Knowledge base 

% student(Name, Group)
student(alisa, 2).
student(bob, 1).
student(chloe, 2).
student(denise, 1).
student(edward, 2).

% friend(Name, Name)
friend(alisa, bob).
friend(alisa, denise).
friend(bob, chloe).
friend(bob, edward).
friend(chloe, denise).
friend(denise, edward).

% parent(Parent, Child)
parent(marjorie, bart).
parent(marjorie, lisa).
parent(marjorie, maggie).
parent(homer, bart).
parent(homer, lisa).
parent(homer, maggie).
parent(abraham, homer).
parent(mona, homer).
parent(jacqueline, marjorie).
parent(jacqueline, patty).
parent(jacqueline, selma).
parent(clancy, marjorie).
parent(clancy, patty).
parent(clancy, selma).
parent(selma, ling).

% unary(Number)
unary(z).
unary(s(X)) :- unary(X).

% Task 1
% Please, check attached file 

% Task 2
% Write down predicate groupmates/2 that checks whether two students are from the same group.
groupmates(X, Y) :- student(X, Z), student(Y, Z).

% Task 3
% Helpers 
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

% Implement predicate relative/2 that checks whether two people are related by blood (share a common ancestor):
relative(X, Y) :- ancestor(Z, X), ancestor(Z, Y).

% Task 4
% Helpers

add(z, Y, Y).
add(s(X), Y, s(R)) :- add(X, Y, R).

% (a) Implement a predicate double/2 that checks if first number is exactly two times the second
double(X, Y) :- add(X, X, Y).

% (b) Implement a predicate leq/2 that checks if the first number is less than or equal to the second numbers:
leq(X, X).
leq(X, s(Y)) :- leq(X, Y).

% (c) Implement multiplication for unary numbers as a predicate mult/3:
mult(z, Y, z).
mult(s(X), Y, Z) :- mult(X, Y, M), add(Y, M, Z).

% (d) Implement a predicate powerOf2/2 such that powerOf2(N, M) is true when M is equal to 2 to the power of N:
powerOf2(z, s(z)).
powerOf2(s(N), M) :- leq(s(z), M), double(Z, M), powerOf2(N, Z).

