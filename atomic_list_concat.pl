:-module(atomic_list_concat,[atomic_list_concat/2]).
atomic_list_concat([H],H).
atomic_list_concat([H1,H2|T],A):-
	atom_concat(H1,H2,H),
	atomic_list_concat([H|T],A).