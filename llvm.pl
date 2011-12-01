:-module(llvm,[llvm/2]).

% atomic_list_concat([H],H):-!.
% atomic_list_concat([H1,H2|T],C):-
% 	atom_concat(H1,H2,H),
% 	atomic_list_concat1([H|T],C).

init:-
	retractall(o(_)),
	retractall(var(_,_,_)),
	retractall(function(_,_,_,_)),
	once(init_(T)),
	assert(o(T)).
init_(File,T):-
	tokenize(File,L),
	parser(L,T),
	llift(T,_F).
init_(T):-
	tokenize(alan,L),
	parser(L,T),
	llift(T,_F).

%the AST is basically a ASF
%each tree of the forest represents a function
%we add a bunch of begin-end stuff and then process each tree alone
%during the proccessing we build a list with the code
%each element of that list is a llvm instruction
%the instruction is represented by a list
%each member of the list represents a basic unit of the instruction (no spaces)
%during the processing two kind of variables may be left non-instantiated:
%variables representing temporary registers
%variables representing types
%at the end of the processing it is not allowed to have non-instantiated type-values
%however, we may have non-instantiated temporary registers
%thus, we create a list with all the registers we may have used when they are created
%and instantiate the variables of the list at the end of processing
%as a final step, we use atomic_list_concat/2 at each list/instruction adding spaces
%and write the instructions of the function at the output
%this way we minimize the memory cost
%it goes without saying that parallelizing the processing is trivial: one process per tree



varcheck(N,T):-
	var(N,T,_) ; true.
	
:-nb_setval(label,1).
typed_make(Result, Operator, Type,Val1, Val2, Code):-
	varcheck(Val1,Type),
	varcheck(Val2,Type),
	new_var(Result),
	Code=[[Result,'=', Operator,
		Type, Val1,',',Val2]].
    
label(LabelPerc, [['br label',LabelPerc],[LR]]):-
	nb_getval(label,N),
	NN is N+1,
	nb_setval(label,NN),
	atom_concat(label,N,Label),
	atom_concat('%',Label,LabelPerc),
	atom_concat(Label,':',LR).

%bool(Boolean_expr,Value,Code)
%true or false
bool([i1,TF],TF,[]).
bool([not,B],Result,Code):-
	bool(B,Val,C1),
	typed_make(Result,xor,i1,Val,1,C2),
	append([C1,C2],Code).

bool([op(OP),B1,B2],Result,Code):-
	bool(B1,Val1,C1),
	%evaluate B1 condition
	C2=[['%llvmvar = alloca i1'], [br,i1,Val1,',',label,L1,',',label,L2]],
	%if true jump to L1 label, otherwise to L2	
	label(L2,C3),
	%label L2
	typed_make(R1, OP,i1,1,0,C4),
	C5=[['store i1', R1, ', i1* %llvmvar'],[br,label,L]],
	%make result true OP false and finish:
	%if op=and the program will be here if B1 is false
	%and the result will be false
	%if op=or the program will be here if B1 is true
	%and the result will be true
	label(L1,C6),
	bool(B2,Val2,C7),
	typed_make(R2,OP,i1,Val1,Val2,C8),
	C9 = [['store i1', R2, ', i1* %llvmvar']],
	%for the other cases the program will be here,
	%evaluate B2 and perform B1 op B2
	label(L,C10),
	load('%llvmvar',i1,Result,C11),
	append([C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11],Code).
	%awesome or what :b

bool([rel(R),E1,E2],Result,Code):-
	expr_eval(E1,V1,Type,C1),
	expr_eval(E2,V2,Type,C2),
	build_op(R,Type,OP),
	typed_make(Result,OP,Type,V1,V2,C3),
	assert(por(C1,C2,C3)),
	append([C1,C2,C3],Code).

    % eq: equal
    % ne: not equal
    % ugt: unsigned greater than
    % uge: unsigned greater or equal
    % ult: unsigned less than
    % ule: unsigned less or equal
    % sgt: signed greater than
    % sge: signed greater or equal
    % slt: signed less than
    % sle: signed less or equal

build_op(R,_,OP):-
	member(R,[eq,ne]),
	atom_concat('icmp ',R,OP).
build_op(R,Type,OP):-
	i_us(Type,US),
	atomic_list_concat(['icmp ', US,R],OP).

i_us(i8,u).
i_us(i16,s).
% perc(N,NP):-
% 	atom_concat('%',N,NP).
convert(V1,T1,V2,T2,C):-
	C = [[V2,'= zext',T1,V1,'to',T2]].
%  <result> = zext <ty> <value> to <ty2>

:-nb_setval(varcounter,1).
new_var(Var):-
	nb_getval(varcounter,N),
	NN is N+1,
	nb_setval(varcounter,NN),
	atomic_list_concat(['%','t',N],Var).

%expr_eval/4 evaluates an expression
%expr_eval(Expression,Value,Type,Code)

%TODO: lval(string)
%TODO: fix lval(array)
	
load(Name,Type,Var,[[Code]]):-
	new_var(Var),
	atomic_list_concat([Var,' = load ',Type,'* ', Name],Code).
	%1 = load i32* %y


expr_eval([lval,Name,[]],Var,Type,Code):-
	var(Name,Type,_),
	load(Name,Type,Var,Code).


%expr: div, rem	
expr_eval([DR,E1,E2],V,Type,C):-
	member(DR,[div,rem]),
	expr_eval(E1,V1,Type,C1),
	expr_eval(E2,V2,Type,C2),
	i_us(Type,US),
	atom_concat(US,div,OP),
	typed_make(V,OP,Type,V1,V2,C3),
	append([C1,C2,C3],C).

%OP: +-* -> add, sub, mul 
expr_eval([OP,E1,E2],V,Type,C):-
	expr_eval(E1,V1,Type,C1),
	expr_eval(E2,V2,Type,C2),
	typed_make(V,OP,Type,V1,V2,C3),
	append([C1,C2,C3], C).

%Type: i8/16
expr_eval([Type,Value],Value,Type,[]).

%i=1? i=val? TODO
	



%1 = call i32 @bar()
expr_eval([func_call,ID,ParList],V,T,C):-
	function(ID,T,TypeVRs,Extras),
	prepare_args(ParList,TypeVRs,Extras,AllArgs,C1),
	new_var(V),
	C2 = [[V,'= call',T,ID|AllArgs]],
	append([C1,C2],C).

prepare_arg(Par,[Type,val],[',',Type,Arg],Code):-
	expr_eval(Par,Arg,Type,Code).
prepare_arg([lval,Name,[]],[Type,ref],[',',RefType,Name],[]):-
	atom_concat(Type,'*',RefType).
	
prepare_extras([Type,Arg],[',',RefType,Arg]):-
	atom_concat(Type,'*',RefType).

prepare_args(Pars,TypeVRs,Extras,SuperFinalArgs,Code):-
	maplist(prepare_arg,Pars,TypeVRs,Args2,Codes),
	append(Args2,Args),
	maplist(prepare_extras,Extras,Extras2),
	append(Extras2,ExtraArgs),
	append(Args,ExtraArgs,AllArgs),
	(AllArgs=[] ; AllArgs=[','| FinalArgs]),
	append([['('],FinalArgs,[')']],SuperFinalArgs),
	append(Codes,Code).


stmt([compound_stmt,Stmts],Code):-
	maplist(stmt,Stmts,Codes),
	append(Codes,Code).

%  call void @bar(i32* %i)
stmt([func_call, ID, ParList],Code):-
	function(ID,void,TypeVRs,Extras),
	prepare_args(ParList,TypeVRs,Extras,AllArgs,C1),
	
	C2 = [[call,void,ID|AllArgs]],
	append([C1,C2],Code).

stmt([if, Cond, Stmt1, Stmt2], Code):-
	bool(Cond,V,C1),
	C2=[[br,i1,V,',',label,L1,',',label,L2]],
	label(L1,C3),
	stmt(Stmt1,C4),
	C5=[[br,label,L]],
	label(L2,C6),
	stmt(Stmt2,C7),
	label(L,C8),
	append([C1,C2,C3,C4,C5,C6,C7,C8],Code).

stmt([if, Cond, Stmt1], Code):-
	bool(Cond,V,C1),
	C2=[[br,i1,V,',',label,L1,',',label,L2]],
	label(L1,C3),
	stmt(Stmt1,C4),
	label(L2,C5),
	append([C1,C2,C3,C4,C5],Code).

stmt([while,Cond,Stmt],Code):-
	label(L,C1),
	bool(Cond,V,C2),
	C3=[[br,i1,V,',',label,L1,',',label,L2]],
	label(L1,C4),
	stmt(Stmt,C5),
	C6 =[[br,label,L]],
	label(L2,C7),
	append([C1,C2,C3,C4,C5,C6,C7],Code).

%TODO: lval array
stmt([equal,[lval,L,[]],E],Code):-
	expr_eval(E,V,T,C1),
	var(L,T,_),
	atomic_list_concat(['store ', T, ' ', V, ', ', T,'* ',L],A),
	append([C1,[[A]]],Code).
%	store i32 %1, i32* %x

%code must always be a list of list(s) of atom(s)
stmt([return,[]],[[ret,void]]).
stmt([return,E],Code):-
	expr_eval(E,Val,Type,C),
	append([C,[[ret,Type,Val]]],Code).



%vars are not defined. there are no local func defs


% define [linkage] [visibility]
%        [cconv] [ret attrs]
%        <ResultType> @<FunctionName> ([argument list])
%        [fn Attrs] [section "name"] [align N]
%        [gc] { ... }


%Arg: [Type,Name]
%VarDef: [Type,Name]

func_def([func_def,ID,Args,RType,VarDefs,Stmts,TypeVRs],Code):-
	declare_args(ID,Args,TypeVRs,C2,NewArgs),
	C1=['define',RType,ID,'('|C2],
	append([C1,[') {']],C3),
	maplist(allocate_arg,NewArgs,Codes0),
	maplist(allocate_var,VarDefs,Codes1),
	append(Codes0,C4),
	append(Codes1,C4a),
	maplist(stmt,Stmts,Codes2),
	append(Codes2H, [Return],Codes2),
	append(Codes2H,C4b),
	maplist(copy_back,NewArgs,Codes3),
	append(Codes3,C4c),
	C5=[['}']],
	append([[C3],C4,C4a,C4b,C4c,Return,C5],Code).

%   %4 = load i32* %b, align 4
%   %5 = load i32** %1, align 8
%   store i32 %4, i32* %5

copy_back([],[]).     %workaround
copy_back([val|_],[]).
copy_back([ref,Name,PointerName,Type],Code):-
	atom_concat(Type,'*',Type2),
	atom_concat(Type2,'*',Type3),
	atom_concat(PointerName,'Reg',PointerNameReg),
	new_var(R4),
	new_var(R5),
	Code = [
		[R4, '= load',Type2,Name],
		[R5, '= load',Type3,PointerNameReg],
		['store',Type,R4,',',Type2,R5]
	       ].

%   define void @bar(i32* %a) nounwind {
%   %1 = alloca i32*, align 8
%   %b = alloca i32, align 4
%   store i32* %a, i32** %1, align 8
%   %2 = load i32** %1, align 8
%   %3 = load i32* %2
%   store i32 %3, i32* %b, align 4

allocate_arg([],[]).  %Workaround
allocate_arg([ref,Name,PointerName,Type],Code):-
	atom_concat(Type,'*',Type2),
	atom_concat(Type2,'*',Type3),
	atom_concat(PointerName,'Reg',PointerNameReg),
	new_var(R2),
	new_var(R3),
	Code = [
		[PointerNameReg,'= alloca',Type2],
		[Name,'= alloca',Type],
		['store', Type2,PointerName,',',Type3,PointerNameReg],
		[R2,'= load', Type3, PointerNameReg],
		[R3, '= load', Type2,R2],
		['store',Type,R3,',',Type2,Name]
	       ].

%  %1 = alloca i32
%  store i32 %i, i32* %1
allocate_arg([val,Name,Name2,Type],Code):-
	atom_concat(Type,'*',Type2),
	Code=[
	      [Name,'= alloca',Type],
	      ['store',Type,Name2,',',Type2,Name]
	     ].
	

declare_args(ID,Args,TypeVRs,Code,ProperRefs):-
	function(ID,_,_,ExtraArgs),
	append(Args,ExtraArgs,AllArgs),
	length(ExtraArgs,N),
	length(ExtraVRs,N),
	(list_to_set(ExtraVRs,[[_,ref]]);ExtraVRs=[]),
	append(TypeVRs,ExtraVRs,AllVRs),
	( AllArgs=[] -> (Code = [], ProperRefs=[])
	; (
	   maplist(declare_arg,AllArgs,AllVRs,[[','|C]|Cs],Refs),   %removes first extra ','
	   append([C|Cs],Code),
	   (Refs=[[]]->ProperRefs=[];
	    ProperRefs=Refs)
	  )
	).

declare_arg([Type,Name],[Type,val],[',',Type,Name2],[val,Name,Name2,Type]):-
	atom_concat(Name,'temp',Name2).
declare_arg([Type,Name],[Type,ref],[',',Type2,Name2],[ref,Name,Name2,Type]):-
	atom_concat(Type,'*',Type2),
	atom_concat(Name,'pointer',Name2).
% %x = alloca i32
allocate_var([Type,Name],[[Name,'= alloca', Type]]).


llvm(T,Code):-
	IncludeLib=[
	['declare void @writeInteger(i16)'],
	['declare void @writeByte(i8)'],
	['declare void @writeChar(i8)'],
	['declare i16 @readInteger()'],
	['declare i8 @readByte()'],
	['declare i8 @readChar()'],
	['declare i8 @shrink(i16)'],
	['declare i16 @extend(i8)']],
	maplist(func_def,T,C),
	append([IncludeLib],C,Code).


		 

