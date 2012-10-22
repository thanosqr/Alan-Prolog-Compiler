:-module(semantic,[semantic/1]).

% Type = int|byte
% Type = table(Size,Type)
% Type = function(Type,Arity,[Types])
% Type = variable(type)
% entry(Name,[var/fun], Type)

% Type Checks:
% array: byte [X], X>0   [ns]

% Function Checks:
% arrays must be passed as references [ns]

% Function Occurence Checks:
% is defined [parser]
% used as command only if defined as a procedure
% used as r-value only if it has a value
% same number of arguments
% real arguments have the same type
% real arguments are l-values of the same type if passed as references
% return(same type)


% Variable Occurence Checks:
% is defined [parser]

% l-side is not a array [ns]
% r-value type = l-value type

% Statement Check:
% if(boolean)
% while(boolean)
% return: check head

%% End of Scope Checks:
%% all variables used
%% all functions used


%%main

%TODO: temp to test
semantic(T):-
	maplist(func_def,T).

%func def
func_def([func_def,_ID,_Args,RType,_VarDef,StmtList,_TypesVR]):-
	maplist(stmt_check(RType),StmtList).

stmt_check(_,[equal,M,E]):-
	M=[lval,L,_]->
	(var(L,T1,_),
	expr_type(E,T2),
	(T1=T2->true
	;error(['l-value ',L,' has type ',
	         T1,' while trying to assign the value of ',
		 E,' which has type ',T2])
	));error(['expected l-value, found ',M]).

stmt_check(X,[return,[]]):-
	X=void;
	error('function does not return a value and is not declared as proc').
stmt_check(void,[return,[_]]):-
	error('function is declared as a proc but returns a value').

stmt_check(T1,[return,E]):-
	E\=[],
	expr_type(E,T2),

	(T1=T2
	;error(['type mismatch: function returns ',T2,'while declared to return ',T1])).

stmt_check(X,[if,C|S]):-
	boolean(C),
	maplist(stmt_check(X),S).


stmt_check(X,[while,C,S]):-
	boolean(C),
	stmt_check(X,S).

stmt_check(RT,[compound_stmt,C]):-
	maplist(stmt_check(RT),C).

stmt_check(_,[func_call,ID,EL]):-
	func_enc(void,ID,EL).

stmt_check(_,_).

%function encounter
func_enc(void,ID,EL):-
	function(ID,RType,Types,_),
	((RType=void);error(['function',ID,' is not declared as proc'])),
	match_type_list(EL,Types).

func_enc(type(TypeR),ID,EL):-
	function(ID,RType,Types,_),
	 ((TypeR=RType)
	 ;error(['type error: function ',ID,' returns ',
		 TypeR,' but is declared to return ',RType])),
	 match_type_list(EL,Types).

match_type_list([],[]).
match_type_list([_|_],[]):-
	error('more arguments than expected').
match_type_list([],[_|_]):-
	error('less arguments than expected').

match_type_list([H|T], [[T1,val]|HT]):-
	expr_type(H,T2),
	(T1=T2->true
	;error(['argument ',H,' has type ',T1,' but ', T2,' was expected'])),
	match_type_list(T,HT).
match_type_list([H|T], [[HA,ref]|HT]):-
	(var(H,HA,_)
	;error(['trying to pass by copy-restore but argument',
		H,' is not a l-value'])),
	match_type_list(T,HT).


expr_type([Op,E1,E2], T1):-
	expr_type(E1,T1),
	expr_type(E2,T2),
	(T1=T2->true
	;error(['in expression ',[Op,E1,E2],' the first arg has type ',T1,' while the second ',T2])).

% expr_type([plus,E], int):-
% 	expr_type(E,int).

% expr_type([minus,E], int):-
% 	expr_type(E,int).


expr_type([E], T):-
	expr_type(E,T).

expr_type([lval,A|_],T1):-
	var(A,T2,_),
	(T1=T2->true
	;error(['type mismatch: lval ',A,' has type ',T2,' while expecting type ',T1])).
expr_type([func_call,ID,B],Type):-
	 func_enc(type(Type),ID,B).
expr_type([T,_V],T).

boolean(X):-
	once(boolean_(X)).
boolean_([i1,1]).
boolean_([i1,0]).
boolean_([not,C]):-
	boolean(C).
boolean_([op(_),C1,C2]):-
	boolean(C1),
	boolean(C2).
boolean_([rel(R),E1,E2]):-
	expr_type(E1,T1),
	expr_type(E2,T2),
	(T1=T2->
	 true;error(['in expression ',[R,E1,E2],' the first arg has type ',T1,' while the second ',T2])).

boolean_(C):-
	error(['condition is not boolean: ',C]).

error(X):-
	nb_setval(serrors,1),
	(er(1)->
	(maplist(print,X),nl);true).
