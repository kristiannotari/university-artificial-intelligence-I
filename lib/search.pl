:- module(search, [solve/3,
		   ds_on/0,
		   ds_off/0,
		   set_strategy/1,
		   get_strategy/1,
		   strategy_request/1,
		   show_strategy/0,
		   search_help/0]).
:- use_module(pl_types).
:- style_check(-discontiguous).
:- maplist(write, [
'\n*******************************************************\n',
'   CARICATO SEARCH: search_help \n',
'**********************************************************']).

unit(search).
%  Unità che accede agli algoritmi di ricerca, per ora
%  limitata a path_search

:- reexport('ga/path_search', [solve/3,
				 ds_on/0,
				 ds_off/0
				]).


:- reexport('ga/path_strategy_manager', [
		                  get_strategy/1,
		                  set_strategy/1,
				  strategy_request/2,
				  check_inf/0]).
:- check_inf.

pred(search, search_help).
%   stampa una breve descrizione delle funzionalità di search

pred(search, show_strategy).
%  stampa le strategie correnti

pred(search, strategy_request(request_descriptor)).
%  strategy_request(?Description, ?Strategy) nondet:
%  associa strategie e descrizioni delle corrispondenti richieste

local_pred(search, write_strategy_request(strategy)).

search_help :-
	nl,
	maplist(writeln,
		[
		 'ALGORITMO DI RICERCA',
		 'search_help:  questo help',
		 'Il problema deve definire:',
		 '    path_search:cost(+N1,?N2,?C)   :  costo di un arco',
		 '    path_search:neighbours(+N,?L)  :  L lista vicini di L',
		 '    path_search:heuristic(+N,?H)   :  H euristica di N',
		 '\nViene fornito:\n',
		 '    solve(+Start, +Goal, -Sol)   : ',
		 '	   a) Start = start(StatoIniz),  Goal = goal(StatoGoal)
			    oppure predicati unari chiamati con call',
		 '	   b) Sol = sol(Goal, Path, Cost)',

		 '\nPer caricare una o più strategie usare:',
		 '   set_strategy(+S) carica le strategie S',
		 '   strategy_request(-S) mostra le strategie implementate',
		 '   show_strategy  mostra la strategia corrente'
		 ]),
	nl.

show_strategy :-
	get_strategy(L),
	(   L=[] ->  writeln('Nessuna strategia caricata')
	;   forall(member(S,L), write_strategy_request(S))).

write_strategy_request(S) :-
	strategy_request(R,S),
	writeln(R).

strategy_request(R) :-
	strategy_request(R,_).










