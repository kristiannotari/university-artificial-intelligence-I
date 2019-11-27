:- consult('rrs/forward_planner').
:- consult('rrs/strips').
:- consult(mondo_macchina).
:- consult(conoscenza_macchina).
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

type([in(luogo),usura(number),pitstop(number),giro(number)]:fluent).
% i fluenti corrispondono ai predicati dinamici del mondo (tranne il tempo che è la quantità da ottimizzare)

local_pred(add_del(action,p_node,list(fluent),list(fluent),number)).
% implementa strips:add_del

local_pred(h(p_node,number)).
% implementa forward_planner:h

add_del(guida(p(S1,T1),V),Stato,Add,Del,Costo) :-
	member(in(p(S0,T0)),Stato),
	sez_succ(S0,S1),
	check_movimento_stato(Stato,V,S1,T1,QU0,QU1,QT0,QT1),
	check_avversario_stato(Stato,S1,T1),
	member(giro(G),Stato),
	raggiungo_pitstop_usura(S1,QU1),
	A0 = [in(p(S1,T1)),usura(QU1)],
	D0 = [in(p(S0,T0)),usura(QU0)],
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

pred(check_avversario_stato(p_node,sezione,traiettoria)).
% verifica se è possibile guidare nel punto p(S,T) in base alla conoscenza post interruzione
% degli avversari nel circuito.
check_avversario_stato(Stato,S,T) :-
	member(giro(G),Stato),
	not(sa_avversario(G,p(S,T))).

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
% raggiungo_pitstop_usura(S,QU): verifica che con l'usura attuale QU e dalla sezione S si possa raggiungere
% 	l'ingresso dei pitstop (per tagliare rami in cui questo non è possibile)
% MODO: det.
raggiungo_pitstop_usura(S,QU) :-
	pitlane_in(S,_)
	;
	sez_succ(S,S1),
	findall(QU0,costo(v2,S1,_,QU0,_),Costi),
	min_list(Costi,QU0min),
	QU >= QU0min,
	QU1 is QU + QU0min,
	usura_massima(QUmax),
	QU1 =< QUmax,
	raggiungo_pitstop_usura(S1,QU1).

%=============================================================================== EURISTICHE

costo_superamento_traguardo_(S,C,QT) :-
	traguardo(S),
	QT is C + 0.
costo_superamento_traguardo_(S,C,QT) :-
	sez_succ(S,S1),
	costo_superamento_traguardo_(S1,C,C1),
	findall(QT0,costo(v1,S,_,_,QT0),Costi),
	min_list(Costi,QT0min),
	QT is C + C1 + QT0min.
costo_superamento_traguardo(Stato,Costo) :-
	member(in(p(S,_)),Stato),
	costo_superamento_traguardo_(S,0,Costo).
costo_superamento_traguardo(Stato,Costo) :-
	member(in(box),Stato),
	Costo is 0.

% (1) EURISTICA distanza ancora da percorrere (in costo)
pesata(W1,W2,Stato,C) :-
	giri(G), member(giro(N), Stato), R is G - N,
	lunghezza_giro(L),
	costo_superamento_traguardo(Stato, QT),
	% member(in(p(S,_)),Stato),
	% maplist(write, ["\nEURISTICA: ", "giro=", N, " rimanenti=", R, ", sezione=", S, ", lg=", L, ", costo=", QT]),
	% numero di giri restanti * lunghezza giro * costo minimo traiettoria (1) + costo (tempo) minimo per questa sezione del circuito
	C is W1 * R * L * 1 + W2 * QT.

% (0) EURISTICA zero
zero(_,0) :- !.

% Esecuzione euristica definita
h(Stato, C) :-
	nb_getval(euristica, E),
	call(E, Stato, C).

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
	maplist(write, ["\nSTATO FINALE: ", Stato]).

% Uso i predicati stato_iniziale/2 e stato_goal/1 per passare stato iniziale e
% stato goal al planner
piano(gareggia,Plan,Cost) :-
	% scelta euristica
	nb_setval(euristica, zero),
	% nb_setval(euristica, pesata(4,4)),
	plan(stato_iniziale,
	     stato_goal,
			     [_Sin|_Path],
			     _Goal,
			     Cost,
				 Plan).