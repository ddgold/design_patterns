male( bob).
male( pat ).
male( cathal ).

female( mary ).
female( ann ).
female( gene ).

parent( bob, pat ).
parent( bob, ann ).
parent( mary, pat ).
parent( mary, ann ).
parent( cathal, bob ).
parent( jane, mary ).
parent( david, mary ).
parent( gene, bob ).

ancestor( X, Y ) :-
	parent( X, Y ).

ancestor( X, Y ) :-
	parent( X, Z ),
	ancestor( Z, Y ).

descendant( X, Y ) :-
	ancestor( Y, X ).

sum_list( [], 0, 0).
sum_list( [Head | Tail], N, X ):-
	sum_list( Tail, N1, X1 ),
	N is N1 + Head,
	X is X1 + 1.
