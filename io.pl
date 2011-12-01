:-module(io,[flush_comment/0,flush_line/0,cread/1,cpeek/1,open/1,error/1, read_till/1, lc/1]).

%%flush nested comments
flush_comment:-
	cread(N1),
	cpeek(N2),
	flush_nested(N1,N2),
	check_end(N1,N2).

check_end('*',')'):-
	cread(_),!.
check_end(N1,N2):-
	(N1\='*' ; N2\=')'),
	flush_comment,!. %\green cut

flush_nested('(','*'):-
	cread(_),
	flush_comment,!.

flush_nested(N1,N2):-
	(N1\='(' ; N2\='*'),!. %\green cut	

%%flush single line comment
flush_line:-
	skip_line(in).

%%read char, throws eof, alters line counter
cread(C):-
    get_char(in,C),
    eof_check(C),
    test_nl(C,1).

eof_check(end_of_file):-
	error('unexpected end of file'),
	lc(L),
	throw(e_eof([t_eof,eof,L])).
eof_check(_).

test_nl('\n',X):-
	add_lc(X).
test_nl(_,_).

%%peek a char
cpeek(C):-
	peek_char(in,C).

%%read till it meets a char (throws eof)
read_till(B):-
	cread(X),
	X=B.
read_till(B):-
	error('unexpected character'),
	read_till(B).


%%file handling
open(X):-
	open(X,read,_,[alias(in)]).

%%error messages
error(X):-
	nb_setval(errors,1),
	er(1)->
	(
	 print('Lexical error: '),
	 print(X),
	 print(' at line '),
	 lc(L),
	 print(L),
	 nl)
	;true.

%% line counter
:-nb_setval(line,1).   %current line

add_lc(X):-
	nb_getval(line, Line),
	NLine is Line+X,
	nb_setval(line, NLine),!.  %\green cut

lc(X):-
	nb_getval(line,X).