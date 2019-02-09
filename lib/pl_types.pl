:- module(pl_types, [check_unit/1,
		     check_unit/2,
		     reconstruct_clauses/2,
		     show_pred_definition/1,
		     show_type_declarations/1,
		     show_pred_declarations/1,
		     show_unit_declaration/1,
		     restart/0,
		     hu/3,
		     bounded_hu/4]).

:- use_module('unit_tr/loader').
:- use_module('unit_tr/tr').
:- use_module('unit_tr/writer').
:- use_module('unit_tr/declaration_syntax').
:- discontiguous pl_types:pred/1.


:- maplist(writeln,[
'\n\n*************************************************************\n',
'Caricato pl_types.   Usare pl_types:help   per un help',
'\n****************************************************************\n'
]).

help:- maplist(writeln, [
'\n\n***********************************************************************\n',
'?- check_unit(Name).  Verifica i tipi della unita'' Name e stampa in output gli errori.',
'?- check_unit(Name,File). Verifica i tipi della unita'' Name e salva in File gli errori.',
'?- reconstruct_clauses(Name, Pred).  Stampa le clausole esaminate con testa Pred',
'      e mostra i tipi inferiti per le variabili',
'?- show_pred_definition(Pred).	Stampa dichiarazione e codice del predicato Pred',
'?- show_type_declarations(Unit).  Stampa le dichiarazioni di tipo di Unit',
'?- show_pred_declarations(Unit).  Stampa le dichiarazioni di predicato di Unit',
'?- show_unit_declaration(Unit).  Stampa la dichiarazione di unità di Unit',
'\n?- hu(Unit, Type, T). Elenca i termini ground T appartenenti all''universo di Herbrand ',
'       del tipo Type dichiarato in Unit. Opera correttamente solo per tipi finiti',
'?- bounded_hu(K, Unit, Type, T). Elenca i termini ground T di profondità <= K appartenenti',
'	all''universo di Herbrand del tipo Type dichiarato in Unit. MODO (++,?,++,--)',
'\n?- restart.   Siccome il type checker usa tabling, puo'' accadere che modifiche apportate',
'    al codice non si trasferiscano nelle tabelle producendo risultati inattesi.',
'    Con restart vengono pulite le tabelle',
'\n?- pl_types:help.   Questo help.',
'\n***********************************************************************\n'
	       ]).

final_errors_msg(Unit) :-
	(   has_errors(Unit); catch(nb_getval(found_errors,true),_,fail)), !,
	writeln('\nFOUND ERRORS')
	;
	writeln('... CHECKED').

print_clauses(Unit, Head) :-
	clear_unit_tables(Unit),
	clear_unit_errors(Unit),
	forall(get_clause(Unit, U1:Head, Body),
	       (   check_clause(Unit, G, U1:Head, Body),
	           ( get_clause_errors(clause(Unit, U1:Head, Body), Errors),
		     show_errors(clause(Unit, U1:Head, Body), Errors), !
		   ; show_clause(G, clause(Unit, U1:Head, Body))))).


pred(check_unit(atom,atom)).
check_unit(Unit,File) :-
	tell(File),
	check_unit(Unit),
	told.

pred(check_unit(atom)).
check_unit(Unit) :-
	(   Unit=user, File = 'user files';  loaded_unit(Unit, File), Unit \= user ),
	maplist(write, ['\n*********  CHECKING UNIT ', Unit,', ', File]),
	nb_setval(found_errors, false),
	forall(decl_error(Unit,_B,C), (nb_setval(found_errors,true), write_decl_error(C))),
	clear_unit_tables(Unit),
        clear_unit_errors(Unit),
	forall(get_clause(Unit, U1:Head, Body),
	       (   check_clause(Unit, _G, U1:Head, Body),
	           ( get_clause_errors(clause(Unit, U1:Head, Body), Errors),!,
		     nb_setval(found_errors,true),
		     show_errors(clause(Unit, U1:Head, Body), Errors)
		   ; true))),
	final_errors_msg(Unit).



reconstruct_clauses(Unit, PN) :-
	ground(PN),
	PN=P/N, !,
	functor(Pred,P,N),
	pred_declaration(Unit, _Decl, Pred),
	functor(Head,P,N),
	print_clauses(Unit, Head).
reconstruct_clauses(Unit, Pred) :-
	pred_declaration(Unit, _Decl, Pred),
	functor(Pred,P,N),
	functor(Head,P,N),
	print_clauses(Unit, Head).

show_specification(File, Unit, Head, Decl) :-
	functor(Decl,Fun, N),
	show_file_declarations(File, Fun/N, Decl),
	(   (Fun = pred; Fun = local_pred) ->
	    print_clauses(Unit,Head)
	;   true).

show_pred_definition(PIn) :-
	unit_pred_pattern(PIn,Unit,Pred),
	pred_declaration(Unit, Decl, Pred),
	functor(Pred,P,N),
	functor(Head,P,N),
	( Unit=user, loaded_user_unit(_,File);  loaded_unit(Unit, File)),
	show_specification(File, Unit, Head, Decl).

unit_pred_pattern(PIn, Unit, Pred) :-
	not(var(PIn)),
	PIn = Unit:P, !,
	ground(Unit),
	pred_pattern(P,Pred).
unit_pred_pattern(PIn, _Unit, Pred) :-
        pred_pattern(PIn,Pred).

pred_pattern(PIn, Pred) :-
	ground(PIn), !,
        PIn=P/N,
	functor(Pred,P,N).
pred_pattern(PIn, PIn).

show_type_declarations(Unit) :-
	loaded_user_unit(Unit,File),
	show_file_declarations(File, type/1).
show_type_declarations(Unit) :-
	loaded_unit(Unit,File),
	show_file_declarations(File, type/1).

show_pred_declarations(Unit) :-
	loaded_user_unit(Unit,File),
	show_file_declarations(File, open_pred/1),
        show_file_declarations(File, pred/1),
	show_file_declarations(File, local_pred/1).
show_pred_declarations(Unit) :-
	loaded_unit(Unit,File),
	show_file_declarations(File, open_pred/2),
        show_file_declarations(File, pred/2),
	show_file_declarations(File, local_pred/2).

show_unit_declaration(Unit) :-
	loaded_user_unit(Unit,File),
	show_file_declarations(File, user_unit/1).
show_unit_declaration(Unit) :-
	loaded_unit(Unit,File),
	show_file_declarations(File, unit/1).













