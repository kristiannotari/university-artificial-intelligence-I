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

type([in(luogo),usura(number),tempo(number),pitstop(number),giro(number)]:fluent).
% i fluenti corrispondono ai predicati dinamici del mondo

local_pred(add_del(action,p_node,list(fluent),list(fluent),number)).
% implementa strips:add_del

local_pred(h(p_node,number)).
% implementa forward_planner:h

add_del(guida(p(S1,T1),V),Stato,Add,Del,Costo) :-
	member(in(p(S0,T0)),Stato),
	sez_succ(S0,S1),
	check_movimento_stato(Stato,V,S1,T1,QU0,QU1,QT0,QT1),
	member(giro(G),Stato),
	A0 = [in(p(S1,T1)),usura(QU1),tempo(QT1)],
	D0 = [in(p(S0,T0)),usura(QU0),tempo(QT0)],
	(
		caso_speciale_guida_stato(G,p(S0,T0),A1,D1),
		append(A0,A1,Add),
		append(D0,D1,Del)
		;
		Add = A0,
		Del = D0
	),
	Costo is QT1 - QT0.
add_del(effettua_pitstop,Stato,Add,Del,Costo) :-
	member(in(p(S0,T0)),Stato),
	pitlane_in(S0,T0),
	pitlane_costo(QT),
	member(tempo(QT0),Stato),
	(
		% se ho completato i giri
		ultimo_giro_stato(Stato) ->
		(
			% -> termino la gara
			Costo is QT / 2,
			QT1 is QT0 + Costo,
			Add = [in(box),tempo(QT1)],
			Del = [in(p(S0,T0)),tempo(QT0)]
		)
		;
		(
			% ; altrimenti effettuo pitstop
			pitlane_out(S1,T1),
			member(usura(QU0),Stato),
			member(pitstop(P0),Stato),
			member(giro(G0),Stato),
			P1 is P0 + 1,
			G1 is G0 + 1,
			QT1 is QT0 + QT,
			Add = [usura(0),tempo(QT1),pitstop(P1),in(p(S1,T1)),giro(G1)],
			Del = [usura(QU0),tempo(QT0),pitstop(P0),in(p(S0,T0)),giro(G0)],
			Costo is QT + (QU0 * 1)
		)
	).
add_del(taglia_traguardo,Stato,[in(box)],[in(p(S,T))],0) :-
	member(in(p(S,T)),Stato),
	traguardo(S),
	ultimo_giro_stato(Stato).

%=============================================================================== UTILS

pred(check_movimento_stato(p_node,velocita,sezione,traiettoria,number,number)).
% come check_movimento di mondo_macchina ma controllo usura come fluente dello stato
check_movimento_stato(Stato,V,S,T,QU0,QU1,QT0,QT1) :-
	member(usura(QU0),Stato),
	member(tempo(QT0),Stato),
	costo(V,S,T,QU,QT),
	QU1 is QU0 + QU,
	QT1 is QT0 + QT,
	usura_massima(Qmax),
	QU1 =< Qmax,
	usura_massima_velocita(V,QmaxV),
	QU1 =< QmaxV.

pred(caso_speciale_guida_stato(number,punto,list,list)).
% caso_speciale_guida_stato(G,P,A,D): dove G è il numero di giri attuale, P il punto
% 	da cui la macchina si muove, A e D le liste di add e del
% MODO: (+,+,-,-,-) det.
% caso speciale guida "partenza da griglia"
caso_speciale_guida_stato(0,p(S0,T0),[giro(1)],[giro(0)]) :-
	griglia(S0,T0).
% caso speciale guida "taglio del traguardo per nuovo giro"
caso_speciale_guida_stato(G0,p(S0,_),[giro(G1)],[giro(G0)]) :-
	G0 =\= 0,
	traguardo(S0),
	G1 is G0 + 1.
	
pred(ultimo_giro_stato(p_node)).
% ultimo_giro(Stato): verifica che sia l'ultimo giro (numero di giri attuale = 
%	numero di giri da effettuare)
% MODO: det.
ultimo_giro_stato(Stato) :-
	member(giro(G),Stato),
	giri(N),
	G =:= N.

%=============================================================================== EURISTICHE

% (1) EURISTICA distanza ancora da percorrere (in costo)
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
	in(L),
	usura(QU),
	tempo(QT),
	giro(G),
	pitstop(P),
	maplist(write, ["\n\nSTATO INIZIALE: ", "in=", L, ", usura=", QU, ", tempo=", QT, ", giro=", G, ", pitstop=", P]),
	list_to_ord_set([in(L),usura(QU),tempo(QT),giro(G),pitstop(P)],Stato).

stato_goal(Stato) :-
	member(in(box),Stato),
	member(usura(QU),Stato),
	usura_massima(Qmax),
	QU =< Qmax,
	member(giro(G),Stato),
	giri(N),
	G =:= N,
	maplist(write, ["STATO FINALE: ", Stato]).

% Uso i predicati stato_iniziale/2 e stato_goal/1 per passare stato iniziale e
% stato goal al planner
piano(gareggia,Plan,Cost) :-
	plan(stato_iniziale,
	     stato_goal,
			     [_Sin|_Path],
			     _Goal,
			     Cost,
				 Plan).