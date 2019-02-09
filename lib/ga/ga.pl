:- module(ga, [solve/3,
	       get_size/2,
	       get_nodes/2,
	       ds_on/0,
	       ds_off/0
	       ]).
:- style_check(-discontiguous).

ds_on :-
	is_ds_on, !
	;
	assert(is_ds_on).

ds_off :-
	retractall(is_ds_on).

:- dynamic(is_ds_on/0).

unit(ga).
%  ga:  acronimo di "generic algorithm"

:- use_module(debug_and_stats).

type(open:p_node).
%  nodi di problema
type(open:f_node).
%  un nodo di frontiera contiene un nodo di problema
%  ed eventuali informazioni addizionali, necessarie per
%  selezionare il nodo, tagliarlo, ecc.
type(open:solution).
%  Le soluzioni
type(open:collection).
%  collezione di f_node, con operazioni di inserimento ed estrazione
type(open:info).
%  informazioni su frontiera e/o nodi
type([fr(info,collection)]:fringe).
%  fr(Info, Active) contiene una collezione di nodi attivi in frontiera
%  ed eventuali informazioni utili per la selezione e inserimento dei
%  nodi di frontiera

open_pred(ga, f_empty(info, fringe)).
%  f_empty(Info, F0) :  F0 frontiera vuota con informazione Info
%  Modo (-) det
open_pred(ga, f_priority(f_node, number)).
% OPZIONALE
% f_priority(N, P) :  P priorità di N in strategie
% che ordinano i nodi in base alla loro priorità
% Ogni strategia che usa f_priority lo deve chiudere
open_pred(ga, f_add(fringe, list(f_node), fringe)).
%  f_add(F1, Exp,F2): aggiunge Exp a F1, originando
%  F2 in base alla strategia
%  MODO: (+,+,,-) det.
open_pred(ga, f_select(fringe, f_node, fringe)).
%   f_select(F1, FN, F2) : seleziona ed estrae da F1 il nodo di
%   espansione corrente, lasciando il resto in F2.
%   Modo: (+,-,-) det.
open_pred(ga, f_size(fringe, integer)).
%   f_size(F,N) : N = num. nodi di F
%   MODO  (++,--) det
open_pred(ga, fringe_to_list(fringe, list(f_node))).
%   fringe_to_list(FR, L) : L = lista dei nodi in FR, nell'ordine
%   MODO (++,--) det
open_pred(ga, expand_nodes(fringe, f_node, list(f_node))).
%  f_expand(Fringe,FN, Exp): espande FN nella lista dei vicini
%  attivi Exp usando eventualmente le informazioni in Fringe
%  MODO  (+,+,-) det
open_pred(ga, start_node(callable, f_node, info)).
%   start_node(StartPred, FN, Info) :   FN è il primo f_node, costruito
%   da StartPred e Info
%   MODO (+,-,-) det.
open_pred(ga, goal_node(fringe, callable, f_node, solution)).
%   f_goal(Fringe, GoalPred, FN, Sol) verifica se FN è una
%   soluzione usando le info in Fringe e GoalPred e restituisce
%   un nodo-soluzione Sol

open_pred(ga, id_increment(info, info)).
%  id_increment(Inf1, Inf2): si ha iterative deepening, viene
%  incrementato il livello memorizzato in Info e si itera la ricerca
%  con i nuovi limiti. SE iterative deepening non è attiva fallisce
%  e non si ha nessuna iterazione [DEFAULT]

%DEFAULTS: non c'è iterative deepening
id_increment(_,_) :- fail.

pred(ga, solve(callable, callable, solution)).
%   solve(Start, Goal, Sol) :   Sol nodo Goal raggiunto da
%   un nodo di start N (cioe' ottenuto con call(Start,N))
%
pred(ga, get_size(fringe,number)).
%  get_size(+F,-N) : N is the size of F
pred(ga, get_nodes(fringe, list(f_node))).
%  get_nodes(+F,-L) : L is the list of nodes of F
%  ordered according to the order in F


%  FLAGS
pred(ga, ds_on).
pred(ga, ds_off).
local_pred(ga, is_ds_on).
% flag




solve(Start, Goal, Sol) :-
	not(is_ds_on),
	start_node(Start, FN, Info),
	f_empty(Info,Empty),
	solve(FN, Empty, Goal, Sol).
solve(Start, Goal, Sol) :-
	is_ds_on,
	das(start_node(Start, FN, Info)),
	das(f_empty(Info,Empty)),
	start_trace(1, start(FN, Empty, Start, Goal)),
	das(solve(FN, Empty, Goal, Sol)).

local_pred(ga, solve(f_node, fringe, callable, solution)).
%   solve(+FN, fr(+Info, +Coll), +Goal, -Sol) nondet :
%   Sol è una soluzione che verifica gli eventuali limiti
%   imposti in Info e che in caso di iterative deepening
%   è ottenuta per incrementi successivi
local_pred(ga, search(f_node, fringe, callable, solution)).
%   search(+FN, fr(+Info, +Coll), +Goal, -Sol) nondet :
%   Sol è una soluzione che verifica gli eventuali limiti
%   imposti in Info

solve(FN, fr(Info, Empty), Goal, Sol) :-
	search(FN, fr(Info, Empty), Goal, Sol);
	id_increment(Info,NewInfo),
        solve(FN, fr(NewInfo, Empty), Goal, Sol).



search(FN, Fringe, Goal, Sol) :-
	goal_node(Fringe, Goal, FN, Sol)
	;
	expand_nodes(Fringe, FN, Expansion),
	f_add(Fringe, Expansion,Fringe2),
	f_select(Fringe2, NextFN, NextFringe),
	search(NextFN, NextFringe, Goal, Sol).


get_size(Fringe, N) :-
	f_size(Fringe,N).
get_nodes(Fringe, Nodes) :-
	fringe_to_list(Fringe,Nodes).

skipped(das/1).
skipped(start_trace/2).
%  escluso type check per il meta-predicato das

das(call(Start, S)) :-
	call(Start,S).
das(start_node(S, FN, Info)) :-
	start_node(S, FN, Info).
das(f_empty(Info,Empty)) :-
	f_empty(Info,Empty).

das(solve(FN, fr(Info, Empty), Goal, Sol)) :-
	das(search(FN, fr(Info, Empty), Goal, Sol));
	das(id_increment(Info,NewInfo)),
	solve_trace(1, solve(FN, fr(NewInfo, Empty), Goal, Sol)),
	das(solve(FN, fr(NewInfo, Empty), Goal, Sol)).

das(search(FN, Fringe, Goal, Sol)) :-
	goal_node(Fringe, Goal, FN, Sol),
	search_trace(1, goal_node(Fringe, Goal, FN, Sol))
	;
	das(expand_nodes(Fringe, FN, Expansion)),
	das(f_add(Fringe, Expansion,Fringe2)),
	das(f_select(Fringe2, NextFN, NextFringe)),
        search_trace(2, search(NextFN, NextFringe, Goal, Sol)),
	das(search(NextFN, NextFringe, Goal, Sol)).

das(id_increment(Info, NewInfo)) :-
	id_increment(Info, NewInfo).

das(expand_nodes(Fringe, FN, Expansion)) :-
	expand_nodes(Fringe, FN, Expansion),
	das_trace(2, expand_nodes(Fringe, FN, Expansion)).

das(f_add(Fringe, Expansion,Fringe2)) :-
	f_add(Fringe, Expansion,Fringe2),
	das_trace(2, f_add(Fringe, Expansion,Fringe2)).

das(f_select(Fringe2, NextFN, NextFringe)) :-
	f_select(Fringe2, NextFN, NextFringe),
	das_trace(2, f_select(Fringe2, NextFN, NextFringe)).





