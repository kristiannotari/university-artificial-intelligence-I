:- module(path_search, [
		       solve/3,
		       get_strategy/1,
	               ds_on/0,
	               ds_off/0
		       ]).
:- style_check(-discontiguous).

unit(path_search).
%  Ricerca di cammini: i nodi di frontiera memorizzano cammino,
%  costo ed euristica.
%  Usa le strategie messe nella

:- use_module(ga, [solve/3,
		   ds_on/0,
		   ds_off/0,
		   get_size/2,
		   get_nodes/2]).
%  ga è l'algoritmo generico; qui si chiudono i tipi aperti
%  f_node e info, in modo da ottenere i cammini e gestire
%  ricerche limitate

:- use_module(path_strategy_manager, [get_strategy/1]).

close_preds(path_search, [ga:start_node/3,
			 ga:goal_node/4,
			 ga:expand_nodes/3,
			 debug_and_stats:write_fringe/2,
			 debug_and_stats:write_node/1,
			 debug_and_stats:write_pnode/1]).

skipped(reverse/2).
skipped(atom_concat/3).
skipped(use_strategy/1).

%==================================================================
%  1.  NODI DI FRONTIERA E SOLUZIONI

type([pn(p_node, list(p_node), number, number)]:f_node).
%   pn(P, Path, Cost, Heur):   nodo di cammino(path node):
%   - P nodo di problema
%   - Path [P|Path] cammino all'inverso
%   - Cost costo del cammino [P|Path]
%   - Heur = euristica h(P)
type([sol(p_node, list(p_node), number)]:solution).
%  sol(G, Path, Cost) :  Path cammino da nodo start
%  a nodo goal G,  con costo Cost

section.
%  2.  Predicati aperti  ================================
%
%	A) Predicati che il problema deve definire
%
open_pred(path_search, neighbours(p_node, list(p_node))).
%  lista dei nodi collegati da archi uscenti nel problema
%
open_pred(path_search, cost(p_node, p_node, number)).
%  OPZIONALE: cost(N1,N2,C):  (N1,N2) arco con  costo C
%  MODO  (+,+,-) det.  Default 1
open_pred(path_search, heuristic(p_node, number)).
%   heuristic(P,H):  H valore euristico di P
%   MODO:  (+,+,-) det
%   OPZIONALE, default 0

%      B)  Predicati aperti legati alle strategie

open_pred(path_search, prune(f_node, p_node)).
%  prune(FN, PN) : l'espansione di FN con il vicino PN è potata.
%  DFEFAULT nessuna potatura:
open_pred(path_search, store_closed(p_node)).
% store_closed(P) : memorizza P nei nodi chiusi, se la strategia di
% potatura dei chiusi è attiva
% DEFAULT successo senza far niente:
open_pred(path_search, update_increment(number)).
%  update_increment(D) implementato da iterative deepening
%  valuta l'incremento minimo per la prossima iterazione
%  DEFAULT successo senza far niente:
open_pred(path_search, start_pruning).
%  start_pruning implementato dalle strategie di potatura di cicli e
%  chiusi. DEFAULT non ci sono chiusi
open_pred(path_search,start_id).
%  start_id implementato da iterative deepening


%  DEFAULTS PER I PREDICATI APERTI DI PROBLEMA
cost(_,_,1).       %costo 1
heuristic(_,0).    %euristica 0

%  DEFAULTS per pruning e iterative deepening
prune(_,_) :- fail.  % nessun pruning
store_closed(_).     % nessun nodo viene messo fra i chiusi
update_increment(_). % nessun incremento in assenza iterative deepening
start_pruning :-
	retractall(path_search:closed(_)).


%  3.  CHIUSURA predicati aperti di ga

%ga:start_node(P,pn(P,[],0, H), BoundStrategy) :-
ga:start_node(Start,pn(P,[],0, H), BoundStrategy) :-
	get_start_node(Start,P),
	start_pruning,
	heuristic(P,H),
	get_strategy(Strategy),!,
	set_f_strategy(Strategy),
	set_p_strategy(Strategy),
	set_b_strategy(Strategy, BoundStrategy).
ga:goal_node(fr(BS,_), Goal, pn(P,Path,Cost,Heur), sol(P,RevPath,Cost)) :-
	check_lower_bounds(BS, pn(P,Path,Cost,Heur)),!,
	is_goal_node(Goal, P),
	reverse([P|Path], RevPath).


ga:expand_nodes(FR, pn(PN,Path,Cost,Heur), Expanded) :-!,
	neighbours(PN,Vicini),
	extend_paths(FR,pn(PN,Path,Cost,Heur),Vicini,[],Expanded),
	store_closed(PN).

% 4.  PREDICATI LOCALI

local_pred(path_search, get_start_node(callable, p_node)).
local_pred(path_search, is_goal_node(callable, p_node)).

local_pred(path_search, check_lower_bounds(info, f_node)).
%   check_bounds(Bounds, FN) :  il livello di FN supera i limiti
%   inferiori sullo spazio di ricerca; per ora solo iterative
%   deepening prevede un limite inferiore
%   MODO  (+,+) semidet
%
local_pred(path_search, extend_paths(fringe, f_node, list(p_node), list(f_node), list(f_node))).
%  extend_paths(FR, FN, LV, Exp1,Exp2) :
%  FN è il nodo di frontiera da espandere mediante i vicini non ancora
%  considerati LV; si ha:
%  Exp2 = Exp1 unito alle estensioni di FN non potate
%  MODO (+,+,+,+,-) det.

local_pred(path_search, set_f_strategy(list(strategy))).
local_pred(path_search, set_p_strategy(list(strategy))).
local_pred(path_search, set_b_strategy(list(strategy), info)).
%  predicati di inizializzazione strategie (f fontiera, p pruning, b
%  bounds)
skipped(use_strategy(any)).
% use_strategy(+S) :  consulta i files che definiscono S
local_pred(path_search,write_problem_node(f_node)).
local_pred(path_search,write_problem_node_nl(f_node)).
local_pred(path_search,write_path(list(p_node))).
%   Predicati di visualizzazione

get_start_node(start(P), S)  :- !,
	  S=P.
get_start_node(Start, P)  :-
	call(user:Start, P).

is_goal_node(goal(P), G)  :- !,
	G=P.
is_goal_node(Goal, P)  :-
	call(user:Goal, P).


check_lower_bounds(no_bound, _) :-!.
check_lower_bounds(id_limit(E,Min,_,_), FN) :-!,
	call(E, FN, L),
	L > Min.
check_lower_bounds(lev_limit(_,_),_FN) :-!.


extend_paths(_,_, [],E, E).
extend_paths(FR, FN, [V|NextV],E1, E2) :-
	prune(FN, V), !,
	%   Taglio dei cicli o dei chiusi
	extend_paths(FR, FN, NextV,E1, E2).
extend_paths(fr(lev_limit(E,Max),Fr), pn(N,Path,Cost, Heur),
	     [V|NextV],Expanded1, Expanded2) :-!,
	cost(N,V,C),
	Cost1 is Cost+C,
	heuristic(V, Heur1),
	call(E, pn(N,Path,Cost,Heur), Level),
	%  Level bounded by Max
	(   Level < Max ->
	    Expanded = [pn(V,[N|Path],Cost1,Heur1)|Expanded1]
	;   Expanded=Expanded1  ),!,
	extend_paths(fr(lev_limit(E,Max),Fr), pn(N,Path,Cost,Heur),
		     NextV, Expanded,Expanded2).
extend_paths(fr(id_limit(E,Min,Max,Up),Fr), pn(N,Path,Cost, Heur),
	     [V|NextV],Expanded1, Expanded2) :-!,
	cost(N,V,C),
	Cost1 is Cost+C,
	heuristic(V, Heur1),
	call(E, pn(N,Path,Cost,Heur), Level),
	%  Level bounded by Max in id
	(   Level =< Max ->
	    Expanded = [pn(V,[N|Path],Cost1,Heur1)|Expanded1]
	;   Increment is Level-Max,
	    update_increment(Increment),
	    Expanded=Expanded1  ),!,
	extend_paths(fr(id_limit(E,Min,Max,Up),Fr), pn(N,Path,Cost,Heur),
		     NextV, Expanded,Expanded2).
extend_paths(fr(BS,Fr), pn(N,Path,Cost, Heur),
	     [V|NextV],Expanded1, Expanded2) :-
	cost(N,V,C),
	Cost1 is Cost+C,
	heuristic(V, Heur1),!,
	extend_paths(fr(BS,Fr), pn(N,Path,Cost,Heur),
		     NextV, [pn(V,[N|Path],Cost1,Heur1)|Expanded1],Expanded2).

set_f_strategy(Strategy) :-
	member(f_strategy(FS), Strategy), !,
	atom_concat('f_strategy/', FS, SClosure),
	use_strategy(SClosure)
	;
	use_strategy('f_strategy/astar').
set_p_strategy(Strategy) :-
	member(p_strategy(FS), Strategy), !,
	atom_concat('p_strategy/', FS, SClosure),
	use_strategy(SClosure)
	;
	use_strategy('p_strategy/no_cut').
set_b_strategy(Strategy, BS) :-
	member(timeout(K), Strategy),!,
	BS = timeout(K),
	nb_setval(timeout, 0),
	use_strategy('id_strategy/no_id')
	;
	member(lev_limit(L, Max), Strategy),!,
	BS = lev_limit(L, Max),
	use_strategy('id_strategy/no_id')
	;
	member(id_limit(L, Min, Max, Up), Strategy),!,
	BS = id_limit(L, Min, Max, Up),
	use_strategy('id_strategy/id'),
	start_id
	;
	BS = no_bound,
	use_strategy('id_strategy/no_id').

use_strategy(S) :-
	module_property(path_search, file(Path)),
	file_directory_name(Path, Dir),
	atomic_list_concat([Dir,'/', S], PS),
	consult(PS).

%=====================================================

debug_and_stats:write_fringe(fr(Info, FR), Show) :-
	get_size(fr(Info, FR), Size),
	maplist(write, ['Frontiera con ', Size, ' nodi\n']),
	(   Show = true ->
	    get_nodes(fr(Info, FR), Nodes),
	    writeln('['),
	    maplist(write_problem_node_nl, Nodes),
	    writeln(']')
	;   true).


debug_and_stats:write_node(pn(PN, Path, Cost, Heur)) :-
	write_problem_node(pn(PN, Path, Cost, Heur)),
	write('\nvuoi il cammino? s_i/ RET : '),
	readln(Risp),
	(   Risp=[s|_]  -> write_path(Path)
	;   true).

debug_and_stats:write_pnode(pn(PN, Path, G, H)) :-
	write_problem_node(pn(PN, Path, G, H)).

write_problem_node(pn(PN, _Path, G, H)) :-
	F is G+H,
	maplist(write, ['   ', PN, ', f:g:h = ', F:G:H]).

write_problem_node_nl(PN) :-
	write_problem_node(PN), nl.

write_path(P) :-
	writeln('['),
	maplist(writeln, P),
	writeln(']').
