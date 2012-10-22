:-module(lexer, [get_token/1,get_tokens/1, tokenize/2]).

% get_token(Token): gets a token from a read-mode stream with alias in
% get_tokens(Tokens): gets a list of all the tokens from a read-mode stream with alias in
%tokenize(File,Tokens): gets a list of all the tokens in the File

% Token: [T_Code, Value, Line number]
% if the token does not have a value, the value is 't'
% the last token is [t_eof, eof, Number of last line]

:-use_module(io).
:-use_module(types).


%first phase: prelexer

prelexer(White,Tkn):-
	whitespace(White),
	pass(void,whitespace,Tkn).

prelexer('(',Tkn):-
	cpeek('*'),
	cread('*'),
	flush_comment,
	pass(void,multi_comment,Tkn).

prelexer('-',Tkn):-
	cpeek('-'),
	flush_line,
	pass(void, line_comment,Tkn).

prelexer('\'',Tkn):-
	cread(C2),
	read_char(C2,Char),
	read_till('\''),
	pass(t_char,Char,Tkn).

prelexer('"',Tkn):-
	set_string_mode(on),
	cread(C),
	read_char(C,Char),
	read_string(Char,String),
	set_string_mode(off),
	pass(t_string,String,Tkn).

prelexer(D,Tkn):-
	digit(D),
	read_number(Num),
	number_chars(NumNum,[D|Num]),
	pass(t_num, NumNum,Tkn).

prelexer(Letter,Tkn):-
%	low_letter(Letter),
	letter(Letter),
	read_word(Letter,Word,Kind),
	pass(Kind,Word,Tkn).

prelexer(C1,Tkn):-
	cpeek(C2),
	pair(C1,C2,Name),
	cread(C2),
	pass(Name,t,Tkn).

prelexer(C1,Tkn):-
	single_char(C1,Name),
	pass(Name,t,Tkn).

prelexer(_,Tkn):-
	error('illegal character'),
	pass(void,illegal,Tkn).

%second phase: individual modes

%%read char
read_char('\\',Char):-
	cread(C),
	read_esc_char(C,Rest),
	concat('\\',Rest,Char).

read_char('"',Char):-
	string_mode(off),
	error('unexpected character while reading a character'),
	cread(C),
	read_char(C,Char).

read_char('"',end_string):-
	string_mode(on).

read_char('\'',Char):-
	error('unexpected character while reading a character'),
	cread(C),
	read_char(C,Char).

read_char(C,C).

%%%read esc char
read_esc_char(C,C):-
	member(C,['n','t','r','0','\\','"','\'']).
read_esc_char(x,Char):-
	read_hex(Hex1),
	read_hex(Hex2),
	concat(Hex1,Hex2,Hex),
	concat(x,Hex,Char).
read_esc_char(_,Char):-
	error('invalid character while reading an escape sequence'),
	cread(C),
	read_esc_char(C,Char).

read_hex(Hex):-
	cread(Hex),
	hex(Hex).
read_hex(Hex):-
	error('invalid character while reading a hex number'),
	read_hex(Hex).

%read string
read_string(end_string,'').
read_string(Char,String):-
	cread(C),
	read_char(C,Char2),
	read_string(Char2,String2),
	concat(Char,String2,String).

%read number
read_number([D|Rest]):-
	cpeek(D),
	digit(D),
	cread(D),
	read_number(Rest).
read_number([]).
	
		
%read word
read_word(Letter,Word,Kind):-
	cread_word(Letter,Word),
	categorize(Word,Kind).

categorize(Word,t_key):-
	keyword(Word).
categorize(_,t_name).

cread_word(Letter,Word):-
	cpeek(L2),
	valid_char(L2),
	cread(L2),
	cread_word(L2,Rest),
	concat(Letter,Rest,Word).
cread_word(L,L).


% third level: pass

pass(TCode,Info,[TCode,Info,Line]):-
	lc(Line).



% interface
tokenize(File,Tokens):-
	io:open(File),
	get_tokens(Tokens),
	close(in).

get_token([t_eof,eof,Line]):-
	cpeek(end_of_file),
	lc(Line),!.
get_token(Token):-
	catch((cread(X),prelexer(X,Tkn)),e_eof(Tkn),true),
	check_void(Tkn,Token),!.

check_void([void,_,_],Token):-
	get_token(Token).
check_void(Token,Token).

get_tokens([t_eof,eof,L],[[t_eof,eof,L]]).
get_tokens(H,[H|L]):-
%	print(H),nl,
	get_token(T),
	get_tokens(T,L).

get_tokens(L):-
	get_token(T),
	get_tokens(T,L),!.


%%string mode

:-nb_setval(string_mode,off).

string_mode(X):-
	nb_getval(string_mode,X).

set_string_mode(X):-
	nb_setval(string_mode,X).

