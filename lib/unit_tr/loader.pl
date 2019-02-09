:- module(loader, [
		   active_unit/3,
		   gen_type/3,
		   restart/0,
		   get_clause/3,
		   gen_pred/3,
		   unit_sub_type/3,
		   skipped_pred/2,
		   clear_unit_tables/1,
		   decl_error/3,
	           loaded_unit/2,
	           loaded_user_unit/2,
	           pred_declaration/3,
	           type_declaration/3,
	           get_pred/3
		  ]).
:- use_module(prolog_types).
:- use_module(library(tabling)).
:- table(unit_sub_type/3).

:- use_module(declaration_syntax).

active_unit(user_unit(U), user, F) :-
	catch(user:user_unit(U),_,fail),
	source_file(user_unit(U), F),
	file_base_name(F,UPL),
	(   UPL=U ; UPL\=U, file_name_extension(U,_,UPL)).
active_unit(unit(U), U, F) :-
	loaded_unit(U,F).

clear_unit_tables(U) :-
	abolish_table_subgoals(unit_sub_type(U,_,_)), !.
clear_unit_tables(_).

restart :- abolish_all_tables.

loaded_unit(U,F) :-
	predicate_property(U:unit(_), file(F)),
	U:unit(U).
loaded_user_unit(U,F) :-
	user_unit(U),
	%predicate_property(user:user_unit(U), file(F)),
        source_file(U:user_unit(U), F),
	unit_name(F,U).

pred_declaration(user, pred(Pred), Pred) :-
	catch(user:pred(Pred),_,fail).
pred_declaration(user, open_pred(Pred), Pred) :-
	catch(user:open_pred(Pred),_,fail).
pred_declaration(user, local_pred(Pred), Pred) :-
	catch(user:local_pred(Pred),_,fail).

pred_declaration(Unit, pred(Unit, Pred), Pred) :-
	loaded_unit(Unit,_File),
	catch(Unit:pred(Unit,Pred),_,fail).
pred_declaration(Unit, open_pred(Unit, Pred), Pred) :-
	loaded_unit(Unit,_File),
	catch(Unit:open_pred(Unit,Pred),_,fail).
pred_declaration(Unit, local_pred(Unit, Pred), Pred) :-
	loaded_unit(Unit,_File),
	catch(Unit:local_pred(Unit,Pred),_,fail).

type_declaration(Unit, type(Type), Type) :-
	loaded_unit(Unit,_File),
	catch(Unit:type(Type),_,fail).
type_declaration(user, type(Type), Type) :-
	catch(user:type(Type),_,fail).

unit_name(F, U) :-
	file_base_name(F, B),
	file_name_extension(U,_,B).

skipped_pred(U, Pred) :-
	(   loaded_unit(U, _); U=user ),
	catch(U:skipped(L), _, fail),
	(  L=P/N ; member(P/N,L)),
	functor(Pred,P,N).

unit_sub_type(U, T, T) :-
	(   get_type(U,_:T); U=prolog, prolog_type(_:T)).
unit_sub_type(U, T1, T2) :-
	gen_sub_type(U,T,T2),
	unit_sub_type(_, T1,T).
gen_sub_type(U, T1, T2) :-
	(   get_type(U, G2:T2); prolog_type(G2:T2)),
	member({T1}, G2),
	(   get_type(_, _:T1); prolog_type(_:T1)).

gen_type(_U, Fun, U1:Type) :-
	get_type(U1, Gen:Type),
	is_list(Gen),
	member(G,Gen),
	generator(G, Fun).
gen_type(U, open, U:Type) :-
	get_type(U,open:Type),
	not((get_type(U, G:Type), G \== open)).

get_type(U, Type) :-
	active_unit(_,U,_),
        catch(U:type(Type),_,fail).

gen_pred(U, prd(P, Args, Types), U1:Pred) :-
	var(Pred),!,
	get_pred(U, U1:PredType,_),
	PredType =.. [P|Args],
	same_length(Types,Args),
	Pred =.. [P|Types].
gen_pred(U, prd(P, Args, Types), U1:Pred) :-
	Pred =.. [P|Args],
	same_length(Types,Args),
	PredType =.. [P|Types],
	get_pred(U, U1:PredType,_).

get_pred(U, U1:Pred, Kind) :-
	U=user, get_user_pred(U1:Pred,Kind)
	;
	get_unit_pred(U,U1:Pred, Kind).

get_unit_pred(U, U1:Pred, Kind) :-
	loaded_unit(U,_),
	(
	    catch(U:pred(U,Pred),_,fail),
	    U1=U, Kind = pred
	    ;
	    catch(U:open_pred(U, Pred),_,fail),
	    U1=U, Kind = open
	    ;
	    catch(U:local_pred(U, Pred), _,fail),
	    U1=U, Kind = local
	    ;
	    catch(U:close_preds(U,PredList),_,fail),
	    member(U1:PN, PredList),
	    pred_pattern(PN, Pred),
	    catch(U1:open_pred(U1,Pred),_,fail),
	    Kind=closed
	    ;
	    predicate_property(U:Pred, imported_from(U1)),
	    catch(U1:pred(U1,Pred),_,fail),
	    Kind=imported
	).
get_user_pred(user:Pred, Kind) :-
	(
	catch(user:pred(Pred),_,fail), Kind = pred
	;
	catch(user:open_pred(Pred),_,fail), Kind = open
	;
	catch(user:local_pred(Pred), _,fail), Kind = local
	).
get_user_pred(U:Pred, Kind) :-
	catch(user:close_preds(PredList),_,fail),
	member(U:PN, PredList),
	pred_pattern(PN, Pred),
	catch(U:open_pred(U,Pred),_,fail), Kind = closed.
get_user_pred(U:Pred, imported) :-
	predicate_property(user:Pred, imported_from(U)),
	catch(U:pred(U,Pred),_,fail).



get_clause(Unit, Unit:Head, Body) :-
	%(   Unit=user, catch(user:pred(Pred),_,fail);
	%    catch(Unit:pred(Unit,Pred), _, fail) ),
	get_pred(Unit, _:Pred, Kind),
	member(Kind,[pred, local, closed]),
	functor(Pred, P, N),
	functor(Head, P, N),
	clause(Unit:Head, Body).
get_clause(Unit, U1:Head, Body) :-
	closing(Unit, U1:Head, Body).

closing(user, U1:Head, Body) :- !,
	catch(user:close_preds(PredList),_,fail),
	member(U1:PN, PredList),
	pred_pattern(PN, Head),
	clause(U1:Head, Body).
closing(Unit, U1:Head, Body) :-
	catch(Unit:close_preds(Unit, PredList),_,fail),
	member(U1:PN, PredList),
	pred_pattern(PN, Head),
	Unit:clause(U1:Head, Body).

pred_pattern(P/N, Pred) :- !,
	functor(Pred, P, N).
pred_pattern(P,P).


%==================   ERRORS

get_decl(user,  Decl, File) :-
	member(Decl, [type(_), pred(_), open_pred(_), local_pred(_), user_unit(_)]),
	clause(user:Decl,true,Ref),
	clause_property(Ref,file(File)).
get_decl(U,  Decl, File) :-
	loaded_unit(U,File),
	get_unit_decl(U, Decl, File).


get_unit_decl(U, unit(U), _).
get_unit_decl(U, Decl, File) :-
	member(Decl, [type(_), pred(U, _), open_pred(U, _), local_pred(U, _)]),
	clause(U:Decl, true, Ref),
	clause_property(Ref,file(File)).


decl_error(U, Decl, declaration_error(Err, context(File, U, Decl))) :-
	get_decl(U, Decl, File),
	check_decl_error(U, Decl, Err).


check_decl_error(_U, type(Type), Err) :-
	type_decl_error(Type, Err).
check_decl_error(_U, type(Gen:_Type),Err) :-
	is_list(Gen),
	member(G, Gen),
	generator(G, fun(_F, _Args, Arity)),
	arity_error(G, Arity, Err).
check_decl_error(user, PredDecl, Err) :-
	member(PredDecl, [pred(Pred), open_pred(Pred), local_pred(Pred)]),
	catch(Pred =.. [_P|Arity], _, Err=not_well_formed_decl(Pred)),
	(   not(var(Err)), !
	;   arity_error(Pred, Arity, Err), !
	;   fail).
check_decl_error(U, PredDecl, Err) :-
	U \= user,
	member(PredDecl, [pred(U,Pred), open_pred(U,Pred), local_pred(U,Pred)]),
	catch(Pred =.. [_P|Arity], _, Err=not_well_formed_decl(Pred)),
	(   not(var(Err)), !
	;   arity_error(Pred, Arity, Err), !
	;   fail).

arity_error(Decl, Arity, arity_error(AT, Decl)) :-
	member(AT,Arity),
	not(var(AT)),
	not(declared_type_term(AT)).


declared_type_term(AT) :-
	catch(functor(AT,T,N),_,fail),
	functor(Type,T,N),
	(   active_unit(_,U,_), get_type(U, _:Type)
	;   is_prolog_type(T/N)).



