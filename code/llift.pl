:-module(llift,[function/4,llift/2,llift/1]).

:-dynamic(function/4).
round_fun(Extra,[ID,Args,Calls],[ID,NewArgs,Calls],A):-
	findall(ArgList,member([ID,ArgList],Extra),L1),
	findall(ArgList,(
		     member(FunId,Calls),
		     member([FunId,ArgList],Extra)
			), L2),
	append(L1,L2,L),
	append(L,SAL),
	(SAL\=[]->
	 (
	  list_to_set(SAL,SALS),
	  exclude(var_(ID),SALS,SALSA),
	  append(SALSA,Args,AllArgs),
	  list_to_set(AllArgs,NewArgs),
	  A=[ID,SALSA]
	 )
	;
	 (
	  NewArgs=Args,
	  A=[]
	 )
	).
	
var_(Scope,ID):-
	var(ID,_,Scope).
	
round([],F,F).
round(Extra,Funs,FinalFuns):-
	maplist(round_fun(Extra),Funs,NewFuns,NewExtra),
	append(NewExtra,NewExtraP),
	round(NewExtraP,NewFuns,FinalFuns).

structure(IDs,FinalFuns):-
	maplist(structure_fun,IDs,Funs,Extra),
	sort([0|Extra],[0|Extras]),
	round(Extras,Funs,FinalFuns).
	

structure_fun(ID,[ID,[],Calls],A):-
	findall(FunX,uses(ID,fun,FunX),Funs),
	sort(Funs,Calls),
	findall(VarX,uses(ID,var,VarX),Vars),
	sort(Vars,VarsS),
	( Vars=0->
	   A=[]
	  ;A=[ID,VarsS]
	).
	

fun_symbol_table(Funs,FinalFuns):-
	maplist(extract_st,Funs,IDs,RTypes,TypeVRs),
	structure(IDs,FinalFuns),
	maplist(fun_st,FinalFuns,RTypes,TypeVRs).
%	assertz(var(Name,_,_):-new_var(Name)). %TOFIX
	compile_predicates([function/4,var/3]). 

fun_st([ID,ExtraArgs,_],RType,TypeVRs):-
	maplist(typed_arg,ExtraArgs,TypedExtraArgs),
	lib(LibFuns),
	maplist(assert,LibFuns),
	assert(function(ID,RType,TypeVRs,TypedExtraArgs)).

typed_arg(Arg,[Type,Arg]):-
	var(Arg,Type,_).
extract_st([func_def,ID,_,RType,_,_,TypeVRs],ID,RType,TypeVRs).


llift(T):-
	llift(T,_).
llift(T,F):-
	once(fun_symbol_table(T,F)).

%assert(function(ID,RType,TypeVRs,TypedExtraArgs)).
lib(LibFuns):-
	LibFuns = [
		   function('@writeInteger',void,[[i16,val]],[]),
		   function('@writeByte',void,[[i8,val]],[]),
		   function('@writeChar',void,[[i8,val]],[]),
		   function('@readInteger',i16,[],[]),
		   function('@readChar',i8,[],[]),
		   function('@readByte',i8,[],[]),
		   function('@extend',i16,[[i8,val]],[]),
		   function('@shrink',i8,[[i16,val]],[])].