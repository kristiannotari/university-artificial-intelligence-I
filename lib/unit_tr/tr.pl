:- module(tr, [hu/3,
	       bounded_hu/4,
	       term/4,
	       atomic/3,
	       gen_type/3,
	       gen_pred/3,
	       check_clause/4,
	       get_clause_errors/2,
	       %show_clause_errors/1,
	       has_errors/1,
	       clear_unit_errors/1
	      ]).

:- use_module(loader,[gen_type/3,
		      gen_pred/3,
		      active_unit/3,
		      get_clause/3,
		      unit_sub_type/3,
		      skipped_pred/2]).
:- use_module(prolog_types).

:- use_module(util).
:- use_module(writer).


hu_unit(U) :-
	active_unit(_,U,_).

hu(Unit,Type,Term) :-
	hu_unit(Unit),
	gen_hu(Unit,Type,Term).
gen_hu(Unit,Type,Term) :-
	gen_f_type(Unit,Fun, U:Type),
	apply_fun(U, Fun, Type, Term).

apply_fun(U, call, Type, Term) :-
	U:call(Type,Term).
apply_fun(U, call(P), _Type, Term) :-
	U:call(P,Term).
apply_fun(U, fun(F,Args,Arity), _Type, Term) :-
	catch(Term =.. [F|Args],_,fail),
	maplist(gen_hu(U), Arity, Args).
apply_fun(U, {T1}, _Type, Term) :-
	gen_hu(U, T1, Term).

gen_f_type(Unit, Fun, U:Type) :-
	not(var(Type)),
	Type = between(_,_),!,
	U = prolog,
	Fun = call
	;
        gen_type(Unit,Fun, U:Type).


bounded_hu(K, Unit,Type,Term) :-
	hu_unit(Unit),
	bounded_gen_hu(K, Unit,Type,Term).

bounded_gen_hu(K, Unit,Type,Term) :-
	K >= 0,
	get_term_gen(Unit, Fun, U:Type, Term),
	H is K-1,
	bounded_apply_fun(H, U, Fun, Type, Term).

bounded_apply_fun(_,U, call, Type, Term) :-
	U:call(Type,Term).
bounded_apply_fun(_,U, call(P), _Type, Term) :-
	U:call(P,Term).
bounded_apply_fun(K, U, fun(F,Args,Arity), _Type, Term) :-
	catch(Term =.. [F|Args],_,fail),
	H is K-1,
	maplist(bounded_gen_hu(H,U), Arity, Args).
bounded_apply_fun(K, U, {T1}, _Type, Term) :-
	H is K-1,
	bounded_gen_hu(H,U, T1, Term).

%================  Term type reconstruction

:- dynamic(clause_error/2).

term(clause(Unit,Head,Body), Cnt, X, T) :-
     var(X),!,
     insert_context(clause(Unit,Head,Body),Unit,X:T, Cnt).
term(_, cnt(C1,C2), Term, Type) :-
     Type==any,!,
     add_term_vars(Term, C1, C2).
term(clause(Unit,Head,Body), Cnt, Term, Type) :-
     get_term_gen(Unit, Fun, U:Type, Term),
     check_fun(clause(Unit,Head,Body), Cnt, Fun, Term, U:Type).

atomic(clause(Unit,Head,Body), cnt(C1,C2), At) :-
	get_atm(At, Vars, Atm),
	insert_vars(Vars,C1,CV),
	(
	get_pred_gen(Unit, prd(_P,Args,Types), _:Atm) *->
	check_args(clause(Unit,Head,Body), cnt(CV,C2), Args, Types)
	;
	assert_once(clause_error(pred_error(cnt(C1,CV), Atm), clause(Unit,Head,Body))),
	fail
	).

get_atm(QAt, Vars, Atm) :-
	drop_qualifiers(QAt, At),
	extract_e_vars(At, Vars, Atm).
extract_e_vars(V^At, [V|Vars], Atm) :- !,
        extract_e_vars(At, Vars, Atm).
extract_e_vars(Atm, [], Atm).

check_fun(Clause, Cnt, {T1}, Term, _) :-!,
	term(Clause, Cnt, Term, T1).
check_fun(Clause,Cnt,call, Term, U:Type) :-!,
	check_call(Clause,Cnt,call, Term, U:Type).
check_fun(Clause,Cnt,call(P), Term, U:Type) :-!,
	check_call(Clause,Cnt,call(P), Term, U:Type).
check_fun(Clause, Cnt, fun(_F,Args,Types),_Term,_Type) :- !,
     check_args(Clause, Cnt, Args, Types).
check_fun(Clause, Cnt, _Fun,Term,Type) :-
     assert_once(clause_error(functor_error(Cnt, Term, Type), Clause)),
     fail.


check_args(_Clause, cnt(G,G), [], []).
check_args(Clause, cnt(G1,G2), [A|AA], [T|TT]) :-
	term(Clause, cnt(G1,G), A, T) *->
	check_args(Clause, cnt(G,G2), AA,TT)
	;
	assert_once(clause_error(type_error(A:T), Clause)),!,
	fail.

check_call(Clause,Cnt,call,Term,U:Type) :-
        var(Term),!,
	insert_context(Clause, U, Term:Type, Cnt).
check_call(_Clause,cnt(G,G),call,Term,U:Type) :-
	%ground(Term), !,
	(   catch(call(U:Type, Term),_,fail) *-> true
	;   %assert_once(clause_error(call_error(cnt(G,G),call(U:Type, Term)),Clause)),
	    fail).

check_call(Clause,Cnt,call(_P),Term,U:Type) :-
        var(Term),!,
	insert_context(Clause, U, Term:Type, Cnt).
check_call(_Clause,cnt(G,G),call(P),Term,U:_Type) :-
	%ground(Term), !,
	(   catch(call(U:P, Term),_,fail) *-> true
	;   %assert_once(clause_error(call_error(cnt(G,G), call(P, Term)), Clause)),
	    fail).


%======================  CLAUSES

check_clause(Unit, G, U1:Head, Body) :-
	retractall(clause_error(_,clause(Unit, U1:Head, Body))),
	check_atomic(head_error(Head),
		     clause(Unit, U1:Head, Body),
		     cnt([],G1), Head),
	drop_qualifiers(Body, Bd),
	check_body(clause(Unit, U1:Head, Body), cnt(G1,G), Bd).


check_atomic(Error, Clause, cnt(C1,C2), A) :-
	atomic(Clause, cnt(C1,C2), A) *->
	true
	;
	assert_once(clause_error(Error, Clause)),
	C1=C2.%,
	%show_errors(Clause).
%check_atomic(_Error, clause(Unit, Head, Body), Cnt, A) :-
%	atomic(clause(Unit, Head, Body), Cnt, A).

check_body(_, cnt(C,C), true) :- !.
check_body(_, cnt(C,C), !) :- !.
check_body(_, cnt(C,C), fail) :- !.
check_body(Clause, cnt(C1,C2),(Bd1,Bd2)) :-!,
	check_body(Clause, cnt(C1,C),Bd1),
	check_body(Clause, cnt(C,C2),Bd2).
check_body(Clause,cnt(C1,C2),(Bd1->Bd2)) :-!,
	check_body(Clause,cnt(C1,C),Bd1),
	check_body(Clause,cnt(C,C2),Bd2).
check_body(Clause,cnt(C1,C2),(Bd1*->Bd2)) :-!,
	check_body(Clause,cnt(C1,C),Bd1),
	check_body(Clause,cnt(C,C2),Bd2).
check_body(Clause,cnt(C1,C2),(Bd1;Bd2)) :-!,
	rename_vars(C1, Bd1, RBd1),
	check_body(Clause,cnt(C1,CC1),RBd1),
	rename_vars(C1, Bd2, RBd2),
	check_body(Clause,cnt(C1,CC2),RBd2),
	extract(C1,CC1,CC2,C2).
check_body(Clause,cnt(C1,C2),forall(Bd1,Bd2)) :-!,
	check_body(Clause,cnt(C1,C),Bd1),
	check_body(Clause,cnt(C,C2),Bd2).
check_body(Clause,cnt(C1,C2),not(Bd)) :-!,
	rename_vars(C1, Bd, RBd),!,
	check_body(Clause,cnt(C1,C2),RBd).
check_body(Clause, cnt(C1,C2), once(Bd1)) :- !,
	check_body(Clause, cnt(C1,C2), Bd1).
check_body(clause(U, _H, _B), cnt(C,C), Skip) :-
	skipped_pred(U, Skip),!.
check_body(Clause, cnt(C1,C2), catch(Bd1, Err, Bd2)) :- !,
	check_body(Clause, cnt(C1,C), Bd1),
	check_error_msg(Clause, C, Err),
	check_body(Clause, cnt(C,C2), Bd2).
check_body(Clause, cnt(C1,C2), asserta(A)) :- !,
	check_assert_retract(Clause, cnt(C1,C2), A).
check_body(Clause, cnt(C1,C2), assertz(A)) :- !,
	check_assert_retract(Clause, cnt(C1,C2), A).
check_body(Clause, cnt(C1,C2), assert(A)) :- !,
	check_assert_retract(Clause, cnt(C1,C2), A).
check_body(Clause, cnt(C1,C2), retract(A)) :- !,
	check_assert_retract(Clause, cnt(C1,C2), A).
check_body(Clause, cnt(C1,C2), retractall(A)) :- !,
	check_assert_retract(Clause, cnt(C1,C2), A).
check_body(Clause, cnt(C1,C2), setof(Term, Bd, Set)) :- !,
	 check_body(Clause, cnt(C1,C), Bd),
	 check_args(Clause, cnt(C,C2), [Term, Set], [Type, list(Type)]).
check_body(Clause, cnt(C1,C2), maplist(Pred,List)) :- !,
	 call_pred(Clause, Pred, [T], cnt(C1,C)),
	 term(Clause, cnt(C,C2), List, list(T)).
check_body(Clause, cnt(C1,C2), maplist(Pred,List1,List2)) :- !,
	 call_pred(Clause, Pred, [T1,T2], cnt(C1,C)),
	 check_args(Clause, cnt(C,C2), [List1, List2], [list(T1), list(T2)]).
check_body(_Clause, cnt(C,C), CALL) :-
	 CALL =.. [call|_], !.  %call ignored
check_body(Clause, cnt(C1,C2), At) :-
	check_atomic(body_atm_error(At), Clause, cnt(C1,C2), At).





%==================  CHECK!!!

call_pred(_, Pred, L, cnt(C, [Pred:call(L)|C])) :-
	var(Pred), !.
call_pred(Clause,Pred, L, cnt(C1,C2)) :-
	Pred =.. [P|Args],
	same_length(AA, L),
	append(Args, AA, ArgsL),
	PredL=..[P|ArgsL],
	add_vars(AA, L, C1,C),!,
	check_atomic(call_error(Pred), Clause, cnt(C,C2), PredL).

check_error_msg(_,_,_).

check_assert_retract(clause(Unit, _Head, _Body), cnt(C1, C2), (A:-B)) :-
	check_atomic(head_error(A),
		     clause(Unit, assert(A:-B), true), cnt(C1,C), A),
	check_body(clause(Unit, assert(A:-B), true), cnt(C,C2), B).
check_assert_retract(clause(Unit, _Head, _Body), cnt(C1, C2), A) :-
	not(A=(_ :- _)),
	check_atomic(head_error(A), clause(Unit, assert(A), true), cnt(C1,C2), A).

add_vars([],[],C,C).
add_vars([X|XX], [T|TT], C1, [X:T|C2]) :-
	add_vars(XX, TT, C1, C2).


add_term_vars(Term, C1, C2) :-
	term_variables(Term, Vars),
	insert_vars(Vars, C1, C2).

insert_vars([], C,C).
insert_vars([X|V], C1, C2) :-
	insert_var(X, C1,C),
	insert_vars(V,C,C2).
insert_var(X,[],[X:_]).
insert_var(X,[Y:T|C], [Y:T|C]) :-
	X==Y,!.
insert_var(X,[Y:T|C1], [Y:T|C2]) :-
	insert_var(X,C1,C2).




%===========================

has_errors(Unit) :-
	clause_error(_,clause(Unit,_,_)),!.

clear_unit_errors(U) :-
	retractall(clause_error(_, clause(U,_,_))).


get_clause_errors(Cl, Errors) :-
	clause_error(_,Cl),
	setof(Err, clause_error(Err,Cl), Errors).

show_clause_errors(Cl) :-
	%  NB.   fails if no clause Cl error
	get_clause_errors(Cl, Errors), !,
	show_errors(Cl, Errors).
	/*(   get_cl_line(Cl, Line, File), ! ; Line = ?, File = ? ),
	ground_vars('X',Cl),
	maplist(write, ['\nERRORS IN CLAUSE AT LINE ', Line, ', file ', File, ':\n']),
	write_clause(2, Cl),
	write('\nErrors:'),
	forall(member(E, Errors),
	       writerror(E)),
	nl.*/

get_cl_line(clause(Unit, U1:Head, Body), Line, Name) :-
	source_file(U1:Head, File),
	file_base_name(File, Name),
	get_clause_line(File, clause(Unit, U1:Head, Body), Line).

assert_warning(A,B) :-
	writeln(warning(A,B)).


%==================  Getting term generators

get_term_gen(Unit,Fun,U:Type,Term) :-
	(   get_functor(Unit,Term, Fun, U:Type)
	;   get_gen(Unit, call, U:Type),
	    Fun = call
	;   get_gen(Unit, call(P), U:Type),
	    Fun = call(P)
	;   get_gen(Unit, {T1}, U:Type),
	    Fun = {T1}).

get_functor(Unit,Term, fun(F,Args,Types), U:Type) :-
	catch(Term =.. [F|Args],_,fail) *->
	F \= {},
	F \= call,
	same_length(Args,Types),
	get_gen(Unit, fun(F,Args,Types), U:Type)
	;
	get_gen(Unit, fun(F,Args,Types), U:Type),
	F \= {},
	F \= call.


get_gen(Unit,Fun,U:Type) :-
	gen_f_type(Unit, Fun, U:Type)
	;
	prolog_gen(Fun,Type),
	U=prolog_types.

%========================  getting pred generators

get_pred_gen(Unit, prd(P,Args,Types), U:Atm) :-
	gen_pred(Unit, prd(P,Args,Types), U:Atm)
	;
	gen_prolog_pred(prd(P,Args,Types), U:Atm).

gen_prolog_pred(PFun, prolog:Pred) :-
	var(Pred), !,
	get_gen_prolog_pred(PFun, Pred).
gen_prolog_pred(PFun, prolog:Pred) :-
	check_gen_prolog_pred(PFun, Pred).

get_gen_prolog_pred(prd(P, Args, Types), Pred) :-
	prolog_pred(PredType),
	%U1 \= open,
	PredType =.. [P|Types],
	same_length(Types,Args),
        Pred =.. [P|Args].

check_gen_prolog_pred(prd(P, Args, Types), Pred) :-
	Pred =.. [P|Args],
	same_length(Types,Args),
	PredType =.. [P|Types],
	prolog_pred(PredType).

%==============   Merging contexts (as difflists)

insert_context(Clause, _U, X:T1, cnt([Y:T|G],[X:T2|G])) :-
	X==Y,!,
	(   merge_types(T1,T,T2) *-> true
	;   assert_once(clause_error(context_error(X:T1, [Y:T|G]), Clause)),
	    fail).
insert_context(Clause, U, X:T1, cnt([Y:T|G1],[Y:T|G2])) :-
	insert_context(Clause, U, X:T1,cnt(G1,G2)).
insert_context(_Clause,_, X:T, cnt([],[X:T])).

%===================  getting common subtypes

merge_types(T,T,T) :- !.
merge_types(T1,T2,T3) :-
	catch((functor(T1, K, N), functor(T2,K,N)), _, fail), !,
	T1 =.. [K|TT1],
	T2 =.. [K|TT2],
	maplist(merge_types, TT1, TT2, TT3),
	T3 =.. [K|TT3].
merge_types(T1,T2,T3) :-
	get_sub_type( T3,T1),
	get_sub_type( T3,T2).


get_sub_type( T, T).
get_sub_type( _T, TAny) :-
	TAny == any.
get_sub_type(T1, T2) :-
	unit_sub_type(_Unit, T1, T2), T1 \= T2.

assert_once(A) :-
	%A,!
	%;
	assert(A).




