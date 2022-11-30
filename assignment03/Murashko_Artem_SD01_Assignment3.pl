% Murashko Artem SD20-01 %
% Programming Paradigms Fall 2022 %
% Homework assignment #3 %

% [1] Binary trees %

% Consider the following representation of binary trees:
%	1. empty — an empty binary tree;
%	2. node(Value, Left, Right) — an node with a value Value and two subtrees (Left and Right)

% Exercise 1.1 
% Predicate tree/1 that checks whether a given term is a valid tree:
tree(empty).
tree(node(_, L, R)) :- tree(L), tree(R).

% Exercise 1.2
% Predicate containedTree/2 such that containedTree(Tree1, Tree2) 
% is true when Tree1 is contained in Tree2:
% 	1. an empty tree is contained in any tree;
% 	2. a non-empty tree is contained in another non-empty tree when they have the same root and both
%	   subtrees are contained in the other subtrees respectively
containedTree(empty, RT) :- tree(RT).
containedTree(node(V, L1, R1), node(V, L2, R2)) :- containedTree(L1, L2), containedTree(R1, R2).

% Exercise 1.3
% Predicate from/2 such that from(Start, List) is true when List is a finite list 
% consisting of consecutive numbers Start, Start+1, Start+2, ...:
from(_, []). 
from(Val, [Val|T]) :- NextVal is Val + 1, from(NextVal, T).

% Exercise 1.4 
% Predicate preorder/2 such that preorder(Tree, List) is
% true when List contains exactly of values from Tree in preorder traversal:
preorder(empty, []).
preorder(node(Val, L, R), [V|T]) :- number(V), !, Val = V, append(T1, T2, T), preorder(L, T1), preorder(R, T2).
preorder(node(V, L, R), [V|T]) :-  preorder(L, T1),  append(T1, T2, T), preorder(R, T2).

% Exercise 1.5
% Predicates leq/2 and less extending built-in comparison
% predicates to work with positive and negative infinities:
leq(-infinity, _) :- !.
leq(_, +infinity) :- !.
leq(_, -infinity) :- !, false.
leq(+infinity, _) :- !, false.
leq(X, Y) :- X =< Y, !.
 
less(X, Y) :- leq(X, Y), X \= Y.

% Predicates geq/2 and greater extending built-in comparison
% predicates to work with positive and negative infinities:
geq(-infinity, _) :- !, false.
geq(_, +infinity) :- !, false.
geq(_, -infinity) :- !.
geq(+infinity, _) :- !.
geq(X, Y) :- X >= Y, !.
 
greater(X, Y) :- geq(X, Y), X \= Y.

% Helpers
% Predicate leqAll/2 that checks that all values in the list 
% is less or equal then the given value:
leqAll(_, []).
leqAll(Val, [H|T]) :- leq(Val, H), leqAll(Val, T).

% Predicate leqAllInTree/2 that checks that all values in the tree 
% is less or equal then the given value:
leqAllInTree(Val, Tree) :- preorder(Tree, Vals), leqAll(Val, Vals).

% Predicate greaterAll/2 that checks that all values in the list 
% is greater then the given value:
greaterAll(_, []).
greaterAll(Val, [H|T]) :- greater(Val, H), greaterAll(Val, T).

% Predicate greaterAllInTree/2 that checks that all values in the tree 
% is greater then the given value:
greaterAllInTree(Val, Tree) :- preorder(Tree, Vals), greaterAll(Val, Vals).

% Exercise 1.6
% Predicate bst/1 that checks that a tree is a binary search tree (BST):
bst(empty) :- !.
bst(node(_, empty, empty)) :- !.
bst(node(Val, node(V, L, R), empty)):- !,
    greaterAllInTree(Val, L), 
    greaterAllInTree(Val, R), 
    greater(Val, V), 
    bst(node(V, L, R)).
bst(node(Val, empty, node(V, L, R))):- !,
    leqAllInTree(Val, L), 
    leqAllInTree(Val, R), 
    leq(Val, V),
    bst(node(V, L, R)).
bst(node(Val, T1, T2)):- !,
    bst(node(Val, T1, empty)),
	bst(node(Val, empty, T2)).

% Exercise 1.7
% Predicate bstInsert/3 such that bstInsert{Value, Before, After}
% is true when After is a binary search tree produced from Before by inserting Value into it:
bstInsert(Val, empty, node(Val, empty, empty)) :- !.
bstInsert(Val, node(V, L1, R), node(V, L2, R)) :- greater(V, Val), bstInsert(Val, L1, L2).
bstInsert(Val, node(V, L, R1), node(V, L, R2)) :- leq(V, Val), bstInsert(Val, R1, R2).

% Exercise 1.8
% Predicate bstMin/2 such that bstMin{Tree, Min} is true 
% when Min is the minimum value stored in Tree:
bstMin(node(Val, empty, empty), Val) :- !.
bstMin(node(_, L, _), Val) :- bstMin(L, Val).
 
% Predicate bstMax/2 such that bstMax{Tree, Max} is true 
% when Max is the maximum value stored in Tree:
bstMax(node(Val, empty, empty), Val) :- !.
bstMax(node(_, _, R), Val) :- bstMax(R, Val).


% [2] Simple expressions %

% Exercise 2.1
% Predicate expr/1 that checks if a given term is a valid arithmetic expression:
%	1. a number;
%	2. a term X + Y where both X and Y are valid expressions;
% 	3. a term X ∗ Y where both X and Y are valid expressions;
expr(X) :- number(X).
expr(X+Y) :- nonvar(X), nonvar(Y), expr(X), expr(Y).
expr(X*Y) :- nonvar(X), nonvar(Y), expr(X), expr(Y).

% Exercise 2.2
% Predicate expr/2 such that expr(Expr, Values) is true when Expr is an 
% expression term that uses each element (number) from Values once (in that order).
% You may assume that Values has known shape (i.e. length):
expr(H, [H]) :- !.
expr(X+Y, L) :- append([H1|T1], [H2|T2], L), expr(X, [H1|T1]), expr(Y, [H2|T2]).
expr(X*Y, L) :- append([H1|T1], [H2|T2], L), expr(X, [H1|T1]), expr(Y, [H2|T2]).

% Exercise 2.3
% Predicate equation/2 such that equation(Values, Result=Expr)
% is true when Expr is an expression term that uses each element (number) 
% from Values once (in that order) and Result is the number equal to the computed value of Expr.
equation(Vals, Res=Expr) :- expr(Expr, Vals), Res is Expr.

% Exercise 2.4
% Predicate equations/2 such that equations(Values, Equations)
% is true when Equations is a list of all distinct equations that can be produced with Values
equations(Vals, Equations) :- equations(Vals, [], Equations).
equations(_, Equations, Equations).
equations(Vals, EquationsLeft, EquationsRight) :-
    equation(Vals, Equation),
    not(member(Equation, EquationsLeft)), !,
    equations(Vals, [Equation|EquationsLeft], EquationsRight).
