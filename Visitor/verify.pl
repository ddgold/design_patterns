/* X = Name of Parent Object */
/* Y = Name of Funtion       */
verify(X, Y):-
	string_concat('accept(', Y, A),	string_concat(A, 'Visitor)', B),
	bcClass(I, true, N, X),
	bcMembers(I, _, _, true, _, _, F),
	write(N), write(' '), write(F), nl.

verify2():-
	bcClass(I, true, 'Visitor.DrawVisitor', _),
	bcMembers(I, _, _, true, _, _, F),
	write(F), nl.