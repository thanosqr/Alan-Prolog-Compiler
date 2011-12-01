:-use_module([lexer,parser,llift,semantic,llvm]).
:-nb_setval(errors,0).
:-nb_setval(serrors,0).

%tokenize(File,TL).
%parser(TL,AST)
%llift(AST)
%var/3, function/4
%semantic(AST).
%llvm(AST,Code)

check_atomic_list_concat:-
	callable(atomic_list_concat/2)->true
	;use_module([atomic_list_concat]).

get_args:-
        current_prolog_flag(argv, Arguments),
        append(_SytemArgs, [--|Args], Arguments),
	run(Args).

partition_Args(R,Apc,As,Ld):-
	(append(T,['-llvmld'|Ld],R)
	;(T=R,Ld=[])),
	(append(Apc,['-llvmas'|As],T)
	;(Apc=R,As=[])).


remove(FN,Ext):-
	atom_concat(FN,Ext,N),
	process_create(path(rm),[N],[]).
			



print_ast(Apc,Filename,AST):-
	once((
	      member('-e',Apc)->true
	     ;(	atom_concat(Filename,'.ast',Fast),
		open(Fast,write,S1,[]),
		maplist(print_ast(S1),AST),
		close(S1)
	      )
	     )
	    ).
print_ast(_,_,_):-
	error(output,warning,'could not print AST properly').

print_llvm(Filename,Code,Fllvm):-
	       atom_concat(Filename,'.llvm',Fllvm),
	       open(Fllvm,write,S2,[]),
	       maplist(print_code(S2),Code),
	       close(S2).
print_llvm(_,_):-
	error(output,warning,'could not print LLVM code properly').

llvm_compile(Filename,Fllvm,As,Ld):-
	once((
	     atom_concat(Filename,'.bc',Fas),
	     catch(process_create(path('llvm-as'),[Fllvm,'-f','-o',Fas|As],[])
		  ,_,throw(critical('llvm-as'))),
	     atom_concat(Filename,'.out',Fout),
	     catch(process_create(path('llvm-ld'),[Fas,'alan.lib.bc','-o',Fout|Ld],[]),
		   _,throw(critical('llvm-ld'))))).
llvm_compile(_):-
	error(output,warning,'could not compile the LLVM assembly or link the bitcode').
	

:-dynamic(er/1).
% er(1) print warnings
run([Filename,'-c'|_]):-
	maplist(remove(Filename),['.ast','.llvm','.bc','.out','.out.bc']).
run(X):-
	catch(
	      catch(run_(X),critical(E),(error(critical,E),fail)),
	      _,(print('swi-prolog runtime error'),fail)).

run_([Filename|R]):-
	once((partition_Args(R,Apc,As,Ld),	
	((member('-s',Apc),assert(er(0)));assert(er(1))))),
	once(((tokenize(Filename,TL);throw(critical(lexer))))),
	once(((parser(TL,AST);throw(critical(parser))))),
	once(((llift(AST);throw(critical(llift))))),
	once(((semantic(AST);error(semantic,fail)))),

	print_ast(Apc,Filename,AST),

	
	nb_getval(errors,ER),
	nb_getval(serrors,SE),
	once((
	      ((ER=0,(SE=0;member('-i',Apc));member('-ii',Apc)))->
	      (
	       once((llvm(AST,Code);throw(critical(llvm)))),
	       print_llvm(Filename,Code,Fllvm),

	       (
		member('-m',Apc)
	       ;llvm_compile(Filename,Fllvm,As,Ld)

		)
	       )
	      
	      )
	     ;fail).



print_ast(S,H):-
	is_list(H),
	maplist(print_ast(S),H),
	nl(S).
print_ast(S,H):-
	print(S,H),
	print(S,'  ').

println(S,X):-
	print(S,X),nl(S).


print_space(A):-
	print(A),
	print(' ').
print_instruction(I):-
	maplist(print_space,I),
	nl.
pc(C):-print_code(C).
print_code(C):-
	maplist(print_instruction,C).

print_space(S,A):-
	print(S,A),
	print(S,' ').
print_instruction(S,I):-
	maplist(print_space(S),I),
	nl(S).

print_code(S,C):-
	maplist(print_instruction(S),C).

mpc(C):-
	maplist(pc,C).


error(semantic,fail):-
	print('warning: semantic check failed. attempting to produce llvm assembly'),
	nl.

error(critical,X):-
	print('critical error: '),
	print('a fatal error occured while running the '),
	print(X),nl.

error(output,warning,X):-
	print('warning: '),
	print(X),nl.