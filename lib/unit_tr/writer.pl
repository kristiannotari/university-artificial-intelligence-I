:- module(writer, [write_clause/2,
		   %write_clause/3,
		   write_context/1,
		   show_clause/2,
		   show_clause/3,
		   writerror/1,
		   show_errors/2,
		   show_errors/3,
		   write_decl_error/1,
		   show_file_declarations/2,
		   show_file_declarations/3
		   ]).

:- use_module(util).
:- use_module(declaration_syntax).



write_clause(H, clause(_U, Head, Body)) :-
	indent(H),
	write_head(Head),
	K is H+10,
	write_if_body(K,Body),
	write('.').

write_context(Gamma) :-
	write(Gamma).

write_head(H) :-
	get_atm(H,A),
	write(A).

write_if_body(_K, true) :- !.
write_if_body(K, Body) :-
	writeln(' :-'),
	write_or(K, Body).
%	write_body(K,Body).

write_or(K, (Bd1;Bd2)) :- !,
	write_imp(K, Bd1),
	nl,
	indent(K),
	writeln(';'),
	write_or(K, Bd2).
write_or(K, Bd) :-
	write_imp(K, Bd).

write_imp(K, (Bd1 -> Bd2)) :- !,
	write_and(K, Bd1),
	writeln('  ->'),
	write_imp(K, Bd2).
write_imp(K, (Bd1 *-> Bd2)) :- !,
	write_and(K, Bd1),
	writeln('  *->'),
	write_imp(K, Bd2).
write_imp(K, Bd) :-
	write_and(K, Bd).

write_and(K, (Bd1, Bd2)) :- !,
	write_and(K, Bd1),
	writeln(','),
	write_and(K, Bd2).
write_and(K, Bd) :- !,
	write_atomic(K, Bd).

write_atomic(K, (Bd1; Bd2)) :- !,
	open_par(K),
	H is K+2,
	write_or(H, (Bd1; Bd2)),
	close_par(K).
write_atomic(K, (Bd1 -> Bd2)) :- !,
	open_par(K),
	H is K+2,
	write_imp(H, (Bd1 -> Bd2)),
	close_par(K).
write_atomic(K, (Bd1 *-> Bd2)) :- !,
	open_par(K),
	H is K+2,
	write_imp(H, (Bd1 *-> Bd2)),
	close_par(K).
write_atomic(K, (Bd1, Bd2)) :- !,
	open_par(K),
	H is K+2,
	write_and(H, (Bd1, Bd2)),
	close_par(K).
write_atomic(K, forall(Bd1, Bd2)) :- !,
	indent(K),
	writeln('forall('),
	H is K+2,
	write_atomic(H, Bd1),
	writeln(' ,'),
	write_atomic(H, Bd2),
	close_par(K).
write_atomic(K, A) :-
	indent(K),
	write(A).

open_par(K) :-
	indent(K),
	writeln('(').

close_par(K) :-
	nl,
	indent(K),
	write(')').

indent(K) :-
	forall(between(1,K,_), write(' ')).

show_clause(G, Cl) :-
	get_cl_line(Cl, Line, File),
	file_base_name(File,ExtName),
	file_name_extension(Name,_,ExtName),
	ground_vars('X',G:Cl),
	maplist(write, ['\n', Name, ':', Line, ' ']),
	write_context(G),
	nl,
	write_clause(2, Cl).
	%,nl.

show_clause(G, Cl, Name) :-
	get_cl_line(Cl, Line, File),
	file_base_name(File,Name),
	ground_vars('X',G:Cl),
	maplist(write, ['\n', Name, ', line ', Line, ': ']),
	write_context(G),
	nl,
	write_clause(2, Cl).

writerror(E) :-
	maplist(write, ['\n    ', E]).

write_decl_error(declaration_error(Err,context(File,_U,Decl))) :-
	get_clause_line(File, clause(_Unit, Decl, true), Line),
	maplist(write, ['\nNOT WELL FORMED DECL. AT LINE ',Line, ', file ',File, ':\n']),
	maplist(write, ['   DECL.:', Decl,
			'\n   ERROR: ', Err,'\n']), !.
write_decl_error(E) :-
	nl,
	writeln(E).


show_errors(Cl, Errors) :-
	(   get_cl_line(Cl, Line, File), ! ; Line = ?, File = ? ),
	ground_vars('X',Cl),
	maplist(write, ['\nERRORS IN CLAUSE AT LINE ', Line, ', file ', File, ':\n']),
	write_clause(2, Cl),
	write('\nErrors:'),
	forall(member(E, Errors),
	       writerror(E)),
	nl.

show_errors(Cl, Errors, File) :-
	(   get_cl_line(Cl, Line, File), ! ; Line = ?, File = ? ),
	ground_vars('X',Cl),
	maplist(write, ['\nERRORS IN CLAUSE AT LINE ', Line, ', file ', File, ':\n']),
	write_clause(2, Cl),
	write('\nErrors:'),
	forall(member(E, Errors),
	       writerror(E)),
	nl.


get_cl_line(clause(Unit, Head, Body), Line, Name) :-
	source_file(Head, File),
	file_base_name(File, Name),
	get_clause_line(File, clause(Unit, Head, Body), Line).



get_decl_format(decl(Decl, '$stream_position'(_,Line,_,_), CList), decl(Decl, Line, Comment)) :-
	get_comment_list(CList, Comment).
write_decl(decl(VDecl, Line, Comment)) :-
	ground_decl(VDecl, Decl),
	maplist(write, ['\nline ', Line, '\n', Decl,'.\n']),
	maplist(writeln, Comment).
write_decl(File, decl(VDecl, Line, Comment)) :-
	ground_decl(VDecl, Decl),
	file_base_name(File, Base),
	maplist(write, ['\n*********** ', Base,
			', at line ', Line, ' ************\n', Decl,'.\n']),
	maplist(writeln, Comment).

ground_decl(Decl, GDecl) :-
	declaration(Decl), !,
	copy_term(Decl, GDecl),
	ground_vars('Type', GDecl).
ground_decl(UnitDecl, UnitDecl).




kind_heading(File, Kind) :- !,
	file_base_name(File,Base),
	kind_msg(Kind, Msg),
	maplist(write, ['\n\n** ', Base|Msg]).
kind_msg('',['']) :- !.
kind_msg(Kind/_,[', ', Kind, '-DECLARATIONS']):- !.
kind_msg(Kind,[' ', Kind]).

show_file_declarations(File, Kind) :-
	get_declarations(File, Kind, Decls),!,
	sort_by_position(Decls, [], SortedDecls),
	kind_heading(File, Kind),
	(   SortedDecls = [],  !
	;   maplist(write_decl, SortedDecls)).

show_file_declarations(File, Kind, Decl) :-
	get_declarations(File, Kind,  Decls),
	member(decl(Decl,Pos,Comm), Decls),
	%kind_heading(File, Kind),
	get_decl_format(decl(Decl,Pos,Comm), FDecl),
	write_decl(File, FDecl).

sort_by_position([], SD,SD).
sort_by_position([Decl|Decls], S1,S2) :-
	get_decl_format(Decl, FDecl),
	insert_by_pos(FDecl, S1, S),
	sort_by_position(Decls,S,S2).

insert_by_pos(Decl, [], [Decl]).
insert_by_pos(decl(Decl, Line, Comment), [decl(Decl1, Line1, Comment1)|Decls],
	      [decl(Decl, Line, Comment), decl(Decl1, Line1, Comment1)|Decls]) :-
	Line < Line1,!.
insert_by_pos(Decl, [Decl1|Decls1], [Decl1|Decls2]) :-
       insert_by_pos(Decl, Decls1, Decls2).

get_comment(_ - Comment, Comment) :- !,
	not(string_concat("%==", _, Comment)).
get_comment(C,C).

get_comment_list([C|CC], [CL|CCL]) :-
	get_comment(C,CL), !,
	get_comment_list(CC,CCL).
get_comment_list(_,[]).



