:-module(parser,[parser/2,uses/3,var/3]).

parser(TokenList,Tree):-
	clearall,
	add_library_funs,
	program(Tree,TokenList,[[t_eof,eof,_]]).
parser(_,_):-
	error('syntax error:possibly in a function\'s definition'),
	nb_setval(errors,1).

program(T)-->
	func_def(T).

func_def([[func_def,ID,Args,RType,VarDef,FStmtList,TypesValRef]|FunDef])-->
	id(fun,ID,IDO),
	{ add(fun,IDO,ID,42),new_scope(ID)},
	t(t_oparen),
%	bracket([fpar_list(T1)],T1),
	fpar_list(Args,TypesValRef),
	t(t_cparen),
	t(t_colon),
	r_type(RType),
	star_local_def(LocalDef),
	compound_stmt([compound_stmt,StmtList]),
	{
	 (RType=void->ReturnStmt=[return,[]];ReturnStmt=[return,[RType,42]]),
	 append(StmtList,[ReturnStmt],FStmtList),
	 clear_scope,
	 partition(is_func_def,LocalDef,FunDef,VarDef)
	}.


          %partition(:Pred, +List, ?Included, ?Excluded)
	  %True if Included contains all elements for which call(Pred, X) succeeds
	  %and Excluded contains the remaining elements. 

is_func_def([func_def|_]).

star_local_def([H|T])-->
	var_def(H),
	star_local_def(T).
star_local_def(D)-->
	func_def(H),
	star_local_def(T),
	{append(H,T,D)}.		%TODO: tail rec
star_local_def([])-->[].

fpar_list([Def|Defs],[TypeVR|TypeVRs])-->
	fpar_def(Def,TypeVR),
	star_fpar_def(Defs,TypeVRs).
fpar_list([],[])-->[].

star_fpar_def([H|T],[TypeVR|TypeVRs])-->
	t(t_comma),
	fpar_def(H,TypeVR),
	star_fpar_def(T,TypeVRs).
star_fpar_def([],[])-->[].

fpar_def([Type,ID],[Type,ValRef])-->
	id(var,ID,IDO),
	t(t_colon),
	(
	 (t(t_key,reference),{ValRef=ref})
	 ;([],{ValRef=val})
	 ),
	type(Type),
	{add(var,IDO,ID,Type)}.

data_type(i16)-->
	t(t_key,int).
data_type(i8)-->
	t(t_key,byte).

type(DT)-->
	data_type(DT).
type(array(DT))-->
	data_type(DT),
	t(t_obracket),
	t(t_cbracket).

r_type(DT)-->
	data_type(DT).
r_type(void)-->
	t(t_key,proc).

% local_def(T)-->
% 	var_def(T);func_def(T).

var_def([Type,ID])-->
	id(var,ID,IDO),
	t(t_colon),
	data_type(DT),
	bracket([t(t_obracket),int_const(T), t(t_cbracket)],T),
	t(t_semi),
	{var_type_inference(DT,T,Type),
	 add(var,IDO,ID,Type)}.

var_type_inference(DT,[],DT).
var_type_inference(DT,X,Table):-
	append([ '[ ',X,' x ', DT, ' ]' ],Table).
%TODO: add suppport for strings

%/ignore empty stmts
s_stmt(X,M,L):-
	once(stmt(X,M,L)).
stmt([])-->
	t(t_semi).

stmt(F)-->
	func_call(F),
	t(t_semi).
stmt([equal,L,E])-->
	l_value(L),
	t(t_equal),
	expr(E),
	t(t_semi).

stmt(C)-->
	compound_stmt(C).

stmt(['if',C,S,T])-->
	t(t_key,'if'),
	t(t_oparen),
	cond(C),
	t(t_cparen),
	s_stmt(S),
	t(t_key,else),
	stmt(T).

stmt(['if',C,S])-->
	t(t_key,'if'),
	t(t_oparen),
	cond(C),
	t(t_cparen),
	stmt(S).

stmt([while,C,S])-->
	t(t_key,while),
	t(t_oparen),
	cond(C),
	t(t_cparen),
	stmt(S).

stmt([return|[E]])-->
	t(t_key,return),
	bracket([expr(E)],E),
	t(t_semi).


stmt(X)-->
 	update_line_number,
	{
	 nb_getval(line,N),
	 error('syntax error',N),
	 nb_setval(errors,1)
	},
 	eat_till_semicolon(Y),
 	( ({Y=42,X=42})
 	; stmt(X)).

eat_till_semicolon(42)-->
	t(t_ccurl).
eat_till_semicolon(0)-->
 	t(t_semi).
eat_till_semicolon(X)-->
 	t(_),
 	eat_till_semicolon(X).

compound_stmt([compound_stmt,S])-->
	t(t_ocurl),
	star_stmt(S).

star_stmt([])-->t(t_ccurl).
star_stmt(R)-->
	stmt(H),
	( ({H=42,R=[]})
	;(star_stmt(T),{R=[H|T]})).



func_call([func_call,ID,B])-->
	id(call,fun,ID,Error),
	update_line_number,
	t(t_oparen),
%	bracket([expr_list(B)],B),
	expr_list(B),
	t(t_cparen),
	{nb_getval(line,N),
	 error(Error,N)}.

expr_list([E|T])-->
	expr(E),
	star_expr(T).
expr_list([])-->[].

star_expr([H|T])-->
	t(t_comma),
	exprR(H),
	star_expr(T).
star_expr([])-->[].

exprR(T)-->
	int_const(T)
	; char_const(T)
	; func_call(T)
	; l_value(T).



exprR(E)-->
	t(t_oparen),
	expr(E),
	t(t_cparen).

%lvl1 precedence
% _ will be filled according to the type of E
exprR([add,[_,0],E])-->
	t(add),
	expr(E).

exprR([sub,[_,0],E])-->
	t(sub),
	expr(E).

%LR fix
expr(E)-->exprR(E).
expr([O,E1,E2])-->
	exprR(E1),
	operator(O),
	expr(E2).

operator(N)-->
%	lvl2 precedence
	t(mul,N)
	; t(div,N)
	; t(rem,N)
%       lvl3 precedence	
	;t(add,N)
	; t(sub,N).

l_value([lval,ID|[T]])-->
	id(call,var,ID,Error),
	update_line_number,
	bracket( [t(t_obracket), expr(T), t(t_cbracket)],T),
	{nb_getval(line,N),
	 error(Error,N)}.

l_value([S])-->
	string_literal(S).

condR([i1, 1])-->
	t(t_key,true,_N).
condR([i1, 0])-->
	t(t_key,false,_N).

condR(C)-->
	t(t_oparen),
	cond(C),
	t(t_cparen).
%lvl1 precedence
condR([not,C])-->
	t(not),
	cond(C).

%lvl4 precedence
condR([rel(R),E1,E2])-->
	expr(E1),
	relation(R),
	expr(E2).

%---LR fix
cond(T)-->
	condR(T).
cond([op(O),C1,C2])-->
	condR(C1),
	bool_op(O),
	cond(C2).



bool_op(N)-->
	%lvl5 precedence
	t(and,N)
	%lvl6 precedence
	; t(or,N).


relation(N)-->
	t(eq,N)
	; t(ne,N)
	; t(lt,N)
	; t(gt,N)
	; t(le,N)
	; t(ge,N).
	

% Mode: fun or var
% we replace all variables and function names with unique IDs
id(call,Mode,IDN,Errors)-->
	id(IDO),
	{ check(Mode,IDO,IDN,Errors)}.

id(Type,IDN,IDO)-->     %Type should be fun or var
	id(IDO),
	{ unify(Type,IDO,IDN)
	}.


id(ID)-->
	t(t_name,_,ID).
int_const([i16,Number])-->
	t(t_num,_,Number).
char_const([i8,Code])-->
	t(t_char,_,Char),
	{Char='\\n'->
	 char_code('\n',Code)
	 ;char_code(Char,Code)}. %convert character to a number
string_literal([string,String])-->
	t(t_string,_,String).


bracket(X,_)-->
	exec(X).
bracket(_,[])-->[].

exec([X])-->
	X.
exec([H|T])-->
	H,
	exec(T).

update_line_number-->
	[[_,_,N]],
	{nb_setval(line,N),
	fail}.
update_line_number-->[].
%used to ask for a token with a certain name
%for example, t(t_cparen)
 t(X)-->
 	[[X,_,_]].


%used to ask for a token with a certain name and returns that name
%for example, t(t_diff,N);t(t_lesser,N)
%t(+,-)
 t(X,X)-->
 	[[X,_,_]].

%used to ask for a token with a certain name and certain value
%for example, t(t_key,true)
%t(+,+)
 t(X,Y)-->
 	[[X,Y,_]].

%used to ask for a token with a certain name and value and return that value
%for example, t(t_num,Number,Number)
%mostly a work-around
 t(X,Y,Y)-->
 	[[X,Y,_]].




% Mini Symbol Table
:-dynamic(entry/4).
:-nb_setval(scope,[global,library]).
:-dynamic(uses/3).
:-dynamic(var/3).


add_library_funs:-
	LibFuns = [
		   entry(library,writeInteger,fun,'@writeInteger'),
		   entry(library,writeByte,fun,'@writeByte'),
		   entry(library,writeChar,fun,'@writeChar'),
		   entry(library,readInteger,fun,'@readInteger'),
		   entry(library,readByte,fun,'@readByte'),
		   entry(library,readChar,fun,'@readChar'),
		   entry(library,shrink,fun,'@shrink'),
		   entry(library,extend,fun,'@extend')],
	maplist(asserta,LibFuns).


add(var,IDO,IDN,Type):-
	nb_getval(scope,[Scope|_]),
	asserta(entry(Scope,IDO,var,IDN)),
	asserta(var(IDN,Type,Scope)).

add(fun,IDO,IDN,_Type):-
	nb_getval(scope,[Scope|_]),
	asserta(entry(Scope,IDO,fun,IDN)).

check(X,Y,Z,Errors):-
	once(check_(X,Y,Z,Errors)).
check_(Mode,IDO,IDN,''):-
	nb_getval(scope,[CScope|_]),
	entry(Scope,IDO,Mode,IDN),
%check if there is IDO and return new ID
% and its scope (scope=ID of fun where it is declared)
	add_use(Scope,CScope,Mode,IDN).

% check_(_Mode,X,X).
check_(Mode,X,X,E):-
	atomic_list_concat(['semantic error: undefined ',Mode,': ',X],E).

add_use(_,CScope,fun,IDN):-
	uses(CScope,fun,IDN)
	;assert(uses(CScope,fun,IDN)).
add_use(S,S,_,_).
add_use(_S,CScope,var,IDN):-
	(uses(CScope,var,IDN)
	;assert(uses(CScope,var,IDN))).


clear_scope:-
	nb_getval(scope,[CScope|Scope]),
	retractall(entry(CScope,_IDO,_,_)),
	nb_setval(scope,Scope).
new_scope(ID):-
	nb_getval(scope,Scope),
	nb_setval(scope,[ID|Scope]).

% unify
:-nb_setval(num,1).
unify(Type,IDO,IDN):-
	((
	  Type=fun,
	  nb_getval(scope,[CScope|_]),
	  entry(CScope,IDO,fun,_),
	  atomic_list_concat(['warning:semantic error:function', IDO,'is already declared'],Error),
	  error(Error))
	;true),
	  nb_getval(num,N),
	  ((N=1,IDNT='main');atom_concat(IDO,N,IDNT)), %first function=main
	  llvm_naming(Type,IDNT,IDN),
	  NN is N+1,
	  nb_setval(num,NN).

llvm_naming(fun,ID,IDN):-
	atom_concat('@',ID,IDN).

llvm_naming(var,ID,IDN):-
	atom_concat('%',ID,IDN).


error('').
error(X):-
	er(1)->(print(X),nl);true.

error(X,N):-
	X=''->true
	;(
	  nb_setval(serrors,2),
	  (er(1)->
	   (
	    print(X),
	    print(' at line '),
	    print(N),nl
	   )
	  ;true)).


clearall:-
	retractall(uses(_,_,_,_)),
	retractall(var(_,_,_)),
	retractall(entry(_,_,_,_)),
	nb_setval(num,1),
	nb_setval(scope,[global,library]).

op(L):-
	maplist(opo(0),L).


opo(X,AL):-
	is_list(AL),
	XX is X+1,
	print('>'),
	maplist(opo(XX),AL).
opo(X,AL):-
	prints(X),
	print(AL),
	nl.
prints(0).
prints(X):-
	print(' '),
	XX is X-1,
	prints(XX).