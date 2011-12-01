:-module(types,[whitespace/1,digit/1,low_letter/1,valid_char/1,hex/1,keyword/1,pair/3,single_char/2, letter/1, cap_letter/1]).

whitespace(X):-
	member(X,[' ', '\n', '\r', '\t']).

digit(D):-
	cbetween('0','9',D).

low_letter(L):-
	cbetween(a,z,L).
cap_letter(L):-
	cbetween('A','X',L).
letter(L):-
	low_letter(L); cap_letter(L).
	

cbetween(_,_,end_of_file):-
	false.
cbetween(L,H,N):-
	N \= end_of_file,
	char_code(L,LN),
	char_code(H,HN),
	char_code(N,NN),
	between(LN,HN,NN).

	
valid_char(C):-
	cbetween(a,z,C).
valid_char(C):-
	cbetween('A','Z',C).
valid_char('_').
valid_char(X):-
	digit(X).


hex(L):-
	cbetween(a,f,L).
hex(L):-
	cbetween('A','F',L).

hex(D):-
	digit(D).

keyword(W):-
	member(W,['byte','return','else','while','false','true','if','int','proc','reference']).

pair(C1,C2,Name):-
	member([C1,C2,Name],[
			  ['=','=', eq],
			  ['!','=', ne],
			  ['<','=', le],
			  ['>','=', ge]
			 ]).

:-op(42,yfy,##).
:-op(42,yfy,@@).
single_char(C,Name):-
	Main##C,
	atom_concat('t_',Main,Name).
single_char(C,Name):-
	Name@@C.

add@@'+'.
mul@@'*'.
rem@@'%'.
div@@'/'.
and@@'&'.
or@@'|'.
not@@'!'.
sub@@'-'.
gt@@'>'.
lt@@'<'.

oparen##'('.
cparen##')'.
obracket##'['.
cbracket##']'.
ocurl##'{'.
ccurl##'}'.
colon##':'.
semi##';'.
comma##','.
equal##'='.






