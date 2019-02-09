:- module(path_strategy_manager, [get_strategy/1,
				  set_strategy/1,
				  strategy_request/2,
				  check_inf/0,
				  f_strategy/1,
				  p_strategy/1
				  ]).

:- style_check(-discontiguous).
skipped(mem_check/2).
skipped(max_check/1).
skipped(inf/1).
skipped(throw/1).
inf(inf).
check_inf:- inf(Inf),
	catch(Inf > 0, _,(   throw(
'L''aritmetica estesa con +inf non e'' implementata nella versione
Prolog da te usata. Devi sostituire nel modulo path_strategy_manager
inf(inf)  con   inf(10e100)'))).


unit(path_strategy_manager).



type([call]:f_strategy).
pred(path_strategy_manager,f_strategy(f_strategy)).
type([call]:p_strategy).
pred(path_strategy_manager, p_strategy(p_strategy)).
type([depth, cost, f]:node_level).
type([no_bound,
      lev_limit(node_level,number),
      id_limit(node_level,number,number,number)]:info).
%  - no_bound: nessun limite di livello
%  - lev_limit(Lev, Max) : Lev(Node) =< Max
%  - id_limit(M,Min,Max,Up) : Min =< M(Node) =< Max =< Up con
%    strato di iterative deepening [Min,Max]
% Chiude il tipo info delle informazioni addizionali in frontiera, per
%  gestire le ricerche limitate in base ad un "misura di livello" Lev.
type([f_strategy(f_strategy), p_strategy(p_strategy),
     {info}]:strategy).

type(['<upper bound>', {number}]: bound_info).

type([{f_strategy}, ps(p_strategy),bs(node_level, bound_info), bs(info),
      id(node_level), id(node_level, bound_info)]:strategy_request).
% bs(info)  limitata a bs(no_bound)

type([atom:strategy_request]:request_descriptor).

local_pred(path_strategy_manager, loaded_strategy(strategy)).
%  loaded_strategy(?S) nondet :  S è una strategia corrente
:- dynamic(loaded_strategy/1).

pred(path_strategy_manager, check_inf).
%  existence check of the numeric constant inf (for infinity)

pred(path_strategy_manager, set_strategy(strategy_request)).
%  set_strategy(+R) det:   R viene aggiunta alle strategie correnti

pred(path_strategy_manager, get_strategy(list(strategy))).
%  get_strategy(?L) nondet:   L lista delle strategie correnti

pred(path_strategy_manager, strategy_request(request_descriptor, strategy)).
%  strategy_request(?Description, ?Strategy) nondet:
%  associa strategie e descrizioni delle corrispondenti richieste

local_pred(path_strategy_manager, clear_id_strategy).
local_pred(path_strategy_manager, strategy_req(request_descriptor)).

%


strategy_request('Frontiera':FS, f_strategy(FS)) :-
	f_strategy(FS).
strategy_request('Pruning':ps(FS), p_strategy(FS)) :-
	p_strategy(FS).
strategy_request('Bounded search':bs(Level, Max1), lev_limit(Level,Max)) :-
	(   var(Max), Max1 = '<upper_bound>' ; number(Max), Max1=Max),
	member(Level, [depth, cost, f]).
strategy_request('Bounded search':bs(no_bound), no_bound).
%%	%%
strategy_request('ID':id(Level), id_limit(Level,_,_,Inf)) :-
	inf(Inf),
	member(Level, [depth, cost, f]).
strategy_request('ID limitato':id(Level, Up1), id_limit(Level, _,_,Up)) :-
	(   var(Up), Up1 = '<upper_bound>' ; not(var(Up)), not(inf(Up)), Up1=Up),
	member(Level, [depth, cost, f]).


f_strategy(depth_first).
f_strategy(breadth_first).
f_strategy(greedy).
f_strategy(lowest_cost).
f_strategy(astar).

p_strategy(cycles).
p_strategy(closed).
p_strategy(no_cut).

mem_check(X, L) :-
	member(X,L), !
	;
	maplist(write, ['\nErrore: ', X, ' non appartiene a ', L,'\n']),
	fail.
max_check(M) :-
	integer(M), M>0, !
	;
	maplist(write,['\n', M, ' limite errato\n']).



set_strategy(FS) :-
	atom(FS),!,
	setof(S, f_strategy(S), SList),
	mem_check(FS, SList),!,
	(   FS=depth_first, ! ; clear_id_strategy),
	retractall(loaded_strategy(f_strategy(_))),
	assert(loaded_strategy(f_strategy(FS))).
set_strategy(ps(PS)) :- !,
	setof(P, p_strategy(P), PList),
	mem_check(PS, PList),
	retractall(loaded_strategy(p_strategy(_))),
	assert(loaded_strategy(p_strategy(PS))).
set_strategy(bs(no_bound)) :- !,

	retractall(loaded_strategy(lev_limit(_,_))),
	retractall(loaded_strategy(id_limit(_,_,_,_))).
set_strategy(bs(Level, Max)) :- !,
        mem_check(Level, [depth, cost, f]),
	max_check(Max),
	retractall(loaded_strategy(lev_limit(_,_))),
	retractall(loaded_strategy(id_limit(_,_,_,_))),
	assert(loaded_strategy(lev_limit(Level,Max))).
set_strategy(id(Level)) :- !,
	mem_check(Level, [depth, cost, f]),
	set_strategy(depth_first),!,
	retractall(loaded_strategy(id_limit(_,_,_,_))),
	retractall(loaded_strategy(lev_limit(_,_))),
	inf(Inf),
	assert(loaded_strategy(id_limit(Level, 0, 0, Inf))).
set_strategy(id(Level,Up)) :- !,
	mem_check(Level, [depth, cost, f]),
	max_check(Up),
	set_strategy(depth_first),!,
	retractall(loaded_strategy(id_limit(_,_,_,_))),
	retractall(loaded_strategy(lev_limit(_,_))),
	assert(loaded_strategy(id_limit(Level,0,0,Up))).
set_strategy(S) :-
	writeln(S:'Errata, le strategie sono'),
	forall(strategy_req(R), writeln(R)).
clear_id_strategy :-
	retractall(loaded_strategy(id_limit(_,_,_,_))),
	retractall(loaded_strategy(lev_limit(_,_))).

strategy_req(R) :-
	strategy_request(R,_).

get_strategy(L) :-
	setof(S, loaded_strategy(S), L), !
	;
	L=[].

%================================

path_search:cost(pn(_N,_Path,Cost,_Heur), Cost).
path_search:depth(pn(_N,Path,_Cost,_Heur), Depth) :-
	length(Path,Depth).
path_search:f(pn(_N,_Path,Cost,Heur), F) :-
	F is Cost+Heur.










