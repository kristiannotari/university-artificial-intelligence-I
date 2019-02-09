:- module(declaration_syntax, [declaration/1,
			       pred_declaration/2,
			       declaration_kind/1,
			       generator/2,
			       pattern/2,
			       same_vars/2,
			       type_decl_error/2,
			       function_symbol/1,
			       check_pred_decl/4]).

declaration(user_unit(_)).
declaration(unit(_)).
declaration(import_types(_)).
declaration(type(_)).
declaration(pred(_)).
declaration(open_pred(_)).
declaration(local_pred(_)).
declaration(pred(_,_)).
declaration(open_pred(_,_)).
declaration(local_pred(_,_)).


pred_declaration(pred(P),P).
pred_declaration(open_pred(P),P).
pred_declaration(local_pred(P),P).
pred_declaration(pred(_,P),P).
pred_declaration(open_pred(_,P),P).
pred_declaration(local_pred(_,P),P).

declaration_kind(Kind) :-
	declaration(Decl),
	functor(Decl, Kind, 1).

function_symbol([]).
function_symbol(F) :-
	atom(F),
	F \= {},
	F \= call.

generator({T}, {T}) :- !.
generator(call, call) :- !.
generator(call(P), call(P)) :- !.
generator(G,fun(F,Args,Arity)) :-
	catch(G =.. [F|Arity],_,fail),
	function_symbol(F),
	same_length(Args, Arity).

pattern(T, Vars) :-
	catch(T=..[_|Vars], _, fail),
	distinct_vars(Vars).

same_vars(Vars1, Vars2) :-
	not((mem(X,Vars1), not(mem(X,Vars2)))),
	not((mem(X,Vars2), not(mem(X,Vars1)))).


type_decl_error(_Gen:T, not(type_pattern(T))) :-
	not(pattern(T, _)).
type_decl_error(Gen:T, not(same_vars(Gen, T))) :-
	pattern(T, Vars1),
	term_variables(Gen,Vars2),
	not(same_vars(Vars1,Vars2)).
type_decl_error(Gen:_T, not(generator_list(Gen))) :-
	Gen \== open,
	not(is_list(Gen)).
type_decl_error(Gen:_T, not(generator(G)) ) :-
	is_list(Gen),
	member(G,Gen),
	not(generator(G, _)).

distinct_vars([]).
distinct_vars([X|W]) :-
	var(X),
	not(mem(X,W)), !,
	distinct_vars(W).

mem(X,[Y|W]) :-
	X==Y, !
	;
	mem(X,W).

type_term(Type, T/N) :-
	catch(Type=..[T|Args], _, fail),
	length(Args,N).

check_pred_decl(Pred, P, Arity, Err) :-
	catch(Pred =.. [P|Arity], _, Err = not_well_formed_declaration(Pred)),
	(    not(var(Err)), !
	;    member(AT, Arity),
	     not(type_term(AT,_)),!,
	     Err = not_well_formed_declaration(Pred)
	;    Err = ok).

