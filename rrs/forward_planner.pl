:- use_module('../lib/search', [get_strategy/1,
		       set_strategy/1,
		       strategy_request/1,
		       solve/3,
		       ds_on/0,
		       ds_off/0,
		       search_help/0
		      ]).
:- use_module('../lib/pl_types').

:- multifile([type/1, pred/1, local_pred/1, open_pred/1, close_preds/1]).
planner_help :-
	maplist(write, [
'A) forward_planner definisce:',
'plan(+StartPred,+GoalPred,-Path,-G,-Plan, -Cost) :  Plan è un piano con',
'    stato iniziale S dato da call(StartPred,S)',
'    stato goal G riconosciuto da call(GoalPred,G)',
'    cammino Path da S a G e piano Plan con coso Cost',
'\nB) Usa il modulo search e deve implementare i tipi predicati aperti di',
'path_search:',
'   path_search:neighbours(+S, -V)  :  V lista dei vicini di S',
'   path_search:cost(S1,S2,C) : C costo della transizione da S1 a S2',
'   path_search:heuristic(S,H) :  H = h(S)'
	       ]).

:- maplist(write, [
'\n\n*************************************************************\n',
' CARICATO FORWARD PLANNER: planner_help \n',
'***************************************************************\n']).

user_unit(forward_planner).
%A)forward_planner definisce:
%    plan(+StartPred,+GoalPred,-Path,-G,-Plan, -Cost) : Plan è un piano
%    con stato iniziale S dato da call(StartPred,S), stato goal G
%    riconosciuto da call(GoalPred,G), cammino Path da S a G e piano
%    Plan con costo Cost, MODO nondet
%
% B) lascia aperti i tipi e predicati:
%    type(open:p_node): gli stati nello spazio di ricerca del
%    pianificatore, importato da path_search
%    type(open:action) : le azioni
%    open_pred(p_do(action,p_node,p_node,number) :
%    p_do(?A, +S1, ?S2, ?C) :  A manda S1 in S2 con costo C
%
% C) Deve chiudere i tipi e predicati aperti di path_search:
%    path_search:neighbours(+S, -V) : V lista dei vicini di S
%    path_search:cost(S1,S2,C) : C costo della transizione da S1 a S2
%    path_search:heuristic(S,H) : H = h(S)

close_preds([path_search:neighbours/2,
	    path_search:cost/3,
	    path_search:heuristic/2]).
% chiude i predicati del modulo path_search per la ricerca in grafi

%============================================================

:- write('\nstrategia frontiera A*'), set_strategy(astar).
:- writeln('potatura chiusi'), set_strategy(ps(closed)).
%:- write('\npotatura cicli'), set_strategy(ps(cycles)).

type(open:action).

open_pred(h(p_node,number)).
%   h(St, H) :   H = h(St), dove h è l'euristica
%
open_pred(p_do(action, p_node, p_node, number)).
%   p_do(A, S1, S2, C) :  l'azione A fa passare da S1 a S2 con
%   costo C;
% MODO (?,++,?,?) nondet:   in particolare, dato uno stato xx, la query
%   p_do(_,xx,V,_) fornisce non deterministicamente i vicini V di xx
% MODO (--,++,++,--) semidet: dati die stati xx e yy, la query
%   p_do(Act,xx,yy,_) restituisce l'unica azione che manda xx in yy
%   o fallisce se non vi sono azioni che mandano xx in yy
%
pred(plan(callable, callable, list(p_node), p_node, number, list(action))).
%   plan(StartPred, GoalPred, Path, Goal, Cost, Plan) :
%   Path è un cammino nel grafo di pianificazione di costoCost che fa
%   passare da uno stato iniziale S ottenuto con call(StartPred, S) a
%   uno stato goal G riconosciuto con call(GoalPred, G)
%   Plan è il piano (sequenza di azioni) corrispondente
%   MODO (+,+,--,--,--,--) nondet

local_pred(states_to_actions(list(p_node), list(action))).
%  states_to_actions(Path, Plan) : Precondizione: Path è un cammino
%  nello spazio di pianificazione, ovvero per ogni coppia di stati
%  consecutivi di Path S1,S2, vi è una azione che manda S1 in S2.
%  Postcondizione: Plan è la sequenza di azioni che produce il cammino
%  Path
%  MODO (++,--) det

path_search:neighbours(S, V) :-
	setof(X, A^C^p_do(A,S,X,C), V), !
	;
	V=[].

path_search:cost(S1,S2,C) :-
	p_do(_, S1, S2, C).

path_search:heuristic(S,H) :-
	h(S,H).


plan(StartPred, GoalPred, Path, Goal, Cost, Plan) :-
	solve(StartPred,
	      GoalPred,sol(Goal,Path,Cost)),
	states_to_actions(Path,Plan).

states_to_actions([S1,S2|P], [A|Acts] ) :-
	p_do(A, S1, S2, _),
	states_to_actions([S2|P], Acts).
states_to_actions([_],[]).


