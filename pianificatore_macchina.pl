:- consult('rrs/forward_planner').
:- consult('rrs/strips').
:- consult(mondo_macchina).
:- multifile([type/1, pred/1, local_pred/1, open_pred/1, skipped/1, user_unit/1]).

user_unit(pianificatore_macchina).
% Pianificazione delle decisioni della macchina con strips + forward_planner.
% Fornisce: piano(Decisione, Piano, Costo)
%
% Importa i tipi:
%	- import_type(macchina:decisione_complessa).
%	- import_type(macchina:action).
%
% Implementa i predicati aperti add_del di strips e h di forward_planner.
% Contiene codice per la sperimentazione di euristiche (predicato test/3).

type([in(luogo),usura(number),pitstop(number),giro(number),avversario(atom,sezione,traiettoria)]:fluent).
% i fluenti corrispondono ai predicati dinamici del mondo

local_pred(add_del(action,p_node,list(fluent),list(fluent),number)).
% implementa strips:add_del

local_pred(h(p_node,number)).
% implementa forward_planner:h

add_del(guida(p(S1,T1)),Stato,Add,Del,Costo) :-
	member(in(p(S0,T0)),Stato),
	sez_succ(S0,S1),
	check_usura_stato(Stato,S1,T1,Q0,Q1),
	not(member(avversario(_,S1,T1),Stato)),
	member(giro(G),Stato),
	A0 = [in(p(S1,T1)),usura(Q1)],
	D0 = [in(p(S0,T0)),usura(Q0)],
	(
		caso_speciale_guida_stato(Stato,G,p(S0,T0),A1,D1),
		append(A0,A1,Add),
		append(D0,D1,Del)
		;
		Add = A0,
		Del = D0
	),
	Costo is Q1 - Q0.
% add_del(effettua_pitstop,Stato,Add,Del,Costo) :-
% 	member(in(p(S0,T0)),Stato),
% 	pitlane_in(S0,T0),
% 	pitlane_costo(C),
% 	(
% 		% se ho completato i giri
% 		ultimo_giro_stato(Stato) ->
% 		(
% 			% -> termino la gara
% 			Add = [in(box)],
% 			Del = [in(p(S0,T0))],
% 			Costo is C
% 		)
% 		;
% 		(
% 			% ; altrimenti effettuo pitstop
% 			pitlane_out(S1,T1),
% 			not(member(avversario(_,S1,T1),Stato)),
% 			member(usura(Q),Stato),
% 			member(pitstop(P),Stato),
% 			member(giro(G),Stato),
% 			P1 is P + 1,
% 			G1 is G + 1,
% 			A0 = [usura(0),pitstop(P1),in(p(S1,T1)),giro(G1)],
% 			D0 = [usura(Q),pitstop(P),in(p(S0,T0)),giro(G)],
% 			sposta_avversari_stato(Stato,A1,D1),
% 			append(A0,A1,Add),
% 			append(D0,D1,Del),
% 			Costo is C + Q * 1
% 		)
% 	).
% add_del(taglia_traguardo,Stato,[in(box)],[in(p(S,T))],0) :-
% 	member(in(p(S,T)),Stato),
% 	traguardo(S),
% 	ultimo_giro_stato(Stato).

%=============================================================================== UTILS

pred(check_usura_stato(p_node,sezione,traiettoria,number,number)).
% come check_usura di mondo_macchina ma controllo usura come fluente dello stato
check_usura_stato(Stato,S,T,Q0,Q1) :-
	member(usura(Q0),Stato),
	costo(S,T,Q),
	Q1 is Q0 + Q,
	usura_massima(Qmax),
	Q1 =< Qmax.

pred(caso_speciale_guida_stato(p_node,number,punto,list,list)).
% caso_speciale_guida_stato(Stato,G,P,A,D): dove G è il numero di giri attuale, P il punto
% 	da cui la macchina si muove, A e D le liste di Add e Del
% MODO: (+,-,-,-) semidet.
% caso speciale guida "partenza da griglia"
caso_speciale_guida_stato(_,0,p(S0,T0),[giro(1)],[giro(0)]) :-
	griglia(S0,T0).
% caso speciale guida "taglio del traguardo per nuovo giro"
caso_speciale_guida_stato(Stato,G,p(S0,_),[giro(G1)|A],[giro(G)|D]) :-
	traguardo(S0),
	sposta_avversari_stato(Stato,A,D),
	G1 is G + 1.

pred(sposta_avversari_stato(p_node,list,list)).
% sposta_avversari(Stato,A,D): sposta tutti gli avversari (evitando di farli
% 	andare su dove è adesso la macchina o altri avversari) e dà in output la
%	lista degli avversari con le nuove (A) e vecchie (D) posizioni.
% MODO: (+,-,-) semidet.
sposta_avversari_stato(Stato,A,D) :-
	not(member(avversario(_,_,_),Stato)) ->
		A = [],
		D = []
	;
		setof(avversario(Nome,S,T),member(avversario(Nome,S,T),Stato),D),
		maplist(sposta_avversario_stato(Stato),D,A).

sposta_avversario_stato(Stato,avversario(Nome,S0,_),avversario(Nome,S1,T)) :-
	sez_succ(S0,S1),
	setof(
		(Costo,T1),
		T1^(
			costo(S1,T1,Costo),
			not(member(in(p(S1,T1)),Stato)),
			not(member(avversario(_,S1,T1),Stato))
		),
		[(_,T)|_]
	).
	
pred(ultimo_giro_stato(p_node)).
% ultimo_giro(Stato): verifica che sia l'ultimo giro (numero di giri attuale = 
%	numero di giri da effettuare)
% MODO: semidet.
ultimo_giro_stato(Stato) :-
	member(giro(G),Stato),
	giri(N),
	G =:= N.

%=============================================================================== EURISTICHE

% % (1) EURISTICA distanza ancora da percorrere (in costo)
% h(Stato,C) :-
% 	giri(G), giro(N), R is G - N,
% 	aggregate_all(count, sez_succ(S1,S2), L),
% 	member(in(p(S,T)),Stato), costo(S,T,CT),
% 	% numero di giri restanti * lunghezza giro * costo minimo traiettoria (1) + costo traiettoria attuale
% 	C is R * L * 1 + CT.

h(_,0) :- !.

%=============================================================================== PIANIFICAZIONE E START-GOAL

local_pred(stato_iniziale(list(fluent))).
% stato_iniziale(S): S è l'insieme ordinato di fluenti che corriponde allo
% 	stato del mondo nel momento in cui la macchina pianifica la decisione

local_pred(stato_goal(list(fluent))).
% stato_goal(S): S è l'insieme ordinato di fluenti che corriponde allo stato
%	goal nel quale non vi sono giri rimasti da fare, la macchina è arrivata al
%	traguardo e non ha mai avuto un usura maggiore dell'usura massima.
% NOTA: si tratta di uno stato di pianificazione, non dello stato
% 	attuale della macchina

pred(piano(decisione_complessa,list(action),number)).
% piano(Dec,Plan,C): pianifica piano di costo C che attua la decisione Dec
% MODO (++,--,--) nondet

stato_iniziale(Stato) :-
	(
		not(avversario(_,_,_)) ->
		ListaAvversari = [];
		setof(avversario(Nome,S,T),avversario(Nome,S,T),ListaAvversari)
	),

	in(L),
	usura(Q),
	giro(G),
	pitstop(P),
	maplist(write, ["\n\nSTATO INIZIALE: ", "in=", L, ", usura=", Q, ", giro=", G, ", pitstop=", P, "\nAVVERSARI: "]),	
	writeln(ListaAvversari),
	list_to_ord_set([in(L),usura(Q),giro(G),pitstop(P)|ListaAvversari],Stato).

stato_goal(Stato) :-
	member(in(box),Stato),
	member(usura(Q),Stato),
	usura_massima(Qmax),
	Q =< Qmax,
	member(giro(G),Stato),
	giri(N),
	G =:= N,

	setof(avversario(Nome,S,T),member(avversario(Nome,S,T),Stato),D),
	writeln("\n\nFINE"),
	maplist(write, ['AVVERSARI: ', D]),
	maplist(write, ['\nUSURA: ', Q, '/', Qmax, '\n\n']).

% Uso i predicati stato_iniziale/2 e stato_goal/1 per passare stato iniziale e
% stato goal al planner
piano(gareggia,Plan,Cost) :-
	plan(stato_iniziale,
	     stato_goal,
			     [_Sin|_Path],
			     _Goal,
			     Cost,
				 Plan).