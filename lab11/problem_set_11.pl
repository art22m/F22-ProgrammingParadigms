% Murashko Artem SD20-01 %
% Programming Paradigms Fall 2022 %
% Problem Set #11 %

% Exercise 1
% 1.a
subseq([], _).
subseq([H|T1], [H|T2]) :- subseq(T1, T2).
subseq([H|T1], [_|T2]) :- subseq([H|T1], T2).

% 1.b
search([], _, 0).
search([H|T1], [H|T2], 0) :- search(T1, T2, 0).
search(Needle, [_|T2], Pos) :- search(Needle, T2, NextPos), Pos is NextPos + 1, Pos > 0.

% 1.d
suffix(List, List).
suffix(List1, [_|T2]) :- suffix(List1, T2).

% 1.e
repeat(_, []).
repeat(X, [X|T]) :- repeat(X, T).

% Exercise 2
% 2.a
allLEQ(_, []).
allLEQ(X, [H|T]) :- X =< H, allLEQ(X, T).
    
% 2.b
contains(H, [H|_]).
contains(X, [_|T]) :- contains(X, T).

minimum(X, List) :- contains(X, List), allLEQ(X, List).

% 2.c
partition(_, [], [], []).
partition(X, [H1|T1], [H1|T2], List3) :- H1 < X, partition(X, T1, T2, List3).
partition(X, [H1|T1], List2, [H1|T3]) :- H1 > X, partition(X, T1, List2, T3).
partition(X, [H1|T1], List2, List3) :- H1 == X, partition(X, T1, List2, List3).

% 2.d
% medianHelper(X, List, C, D) - Does in the list C (number) elements < X and D elements > X?
medianHelper(_, [], 0, 0).
medianHelper(X, [H|T], LessCnter, HighCnter) :- 
    X < H, 
    medianHelper(X, T, LessCnterNxt, HighCnter),
    LessCnter is LessCnterNxt + 1.
medianHelper(X, [H|T], LessCnter, HighCnter) :- 
    X > H, 
    medianHelper(X, T, LessCnter, HighCnterNxt),
    HighCnter is HighCnterNxt + 1.
medianHelper(X, [H|T], LessCnter, HighCnter) :- 
    X == H, 
    medianHelper(X, T, LessCnter, HighCnter).

% medianHelper2(X, List, List) - Is X is a median of a List. Second list is equal to the first one.
medianHelper2(_, [], []).
medianHelper2(H, [H|_], List) :- medianHelper(H, List, Less, Greater), Less == Greater.
medianHelper2(X, [_|T], List) :- medianHelper2(X, T, List).

median(_, []).
median(X, List) :- medianHelper2(X, List, List).

% Exercise 3
% 3.a
allZeros([]).
allZeros([0|T]) :- allZeros(T).

allOnes([]).
allOnes([1|T]) :- allOnes(T).

listEqualLength([], []).
listEqualLength([_|T1], [_|T2]) :- listEqualLength(T1, T2).

listLengthDifferentByOne([], [_]).
listLengthDifferentByOne([_|T1], [_|T2]) :- listLengthDifferentByOne(T1, T2).

increment([H|T1], [H|T2]) :- listEqualLength(T1, T2), increment(T1, T2).
increment([H1|T1], [H2|T2]) :- 
    listEqualLength(T1, T2),
    allOnes(T1), allZeros(T2), 
    H1 is 0, H2 is 1.
increment([H|T1], [H|T2]) :- 
    listLengthDifferentByOne(T1, T2), 
    allOnes(T1), allZeros(T2), H is 1.

% 3.b
countTrailingZeros(List, X) :- repeat(0, List), length(List, X).
countTrailingZeros([H|T], X) :- not(repeat(0, [H|T])), countTrailingZeros(T, X).

% Exercise 4
fib2(1, 1).
fib2(X, Y) :- fib2(Prev, X), Y is Prev + X.

fib(X) :- fib2(X, _).

