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
% i fluenti corrispondono ai predicati dinamici del mondo

local_pred(add_del(action, p_node, list(fluent), list(fluent), number)).
% implementa strips:add_del

local_pred(h(p_node,number)).
% implementa forward_planner:h

% pred(add_del(action, p_node, list(fluent), list(fluent), number)).
% IMPLEMENTA strips:add_del

% guida dalla griglia (partenza)
add_del(guida(p(S,T)),Stato,[in(p(S,T)),usura(Q1),giro(1)],[in(griglia),usura(0),giro(0)],Costo) :-
	member(in(griglia),Stato),
	sez_succ(griglia,p(S,T)),
	check_usura_stato(Stato,S,T,0,Q1),
	not(member(avversario(_,p(S,T)),Stato)),
	Costo is Q1.
% guida per terminare un giro (giro + sposta avversari)
add_del(guida(p(S,T)),Stato,Add,Del,Costo) :-
	member(in(L),Stato),
	sez_succ(L,traguardo),
	sez_succ(griglia,p(S,T)),
	check_usura_stato(Stato,S,T,Q,Q1),
	sposta_avversari_stato(Stato,A,D),
	not(member(avversario(_,p(S,T)),Stato)),
	member(giro(G),Stato),
	G1 is G + 1,
	append([in(p(S,T)),usura(Q1),giro(G1)],A,Add),
	append([in(L),usura(Q),giro(G)],D,Del),
	Costo is Q1 - Q.
% guida ("normale")
add_del(guida(p(S,T)),Stato,[in(p(S,T)),usura(Q1)],[in(p(S0,T0)),usura(Q)],Costo) :-
	member(in(p(S0,T0)),Stato),
	sez_succ(p(S0,T0),p(S,T)),
	check_usura_stato(Stato,S,T,Q,Q1),
	not(member(avversario(_,p(S,T)),Stato)),
	Costo is Q1 - Q.
add_del(effettua_pitstop,Stato,Add,Del,Costo) :-
	member(in(p(S0,T0)),Stato),
	sez_succ(p(S0,T0),pit),
	sez_succ(pit,p(S1,T1)),
	member(usura(Q),Stato),
	member(giro(G),Stato),
	member(pitstop(N),Stato),
	G1 is G + 1,
	N1 is N + 1,
	Costo is Q * 1,
	sposta_avversari_stato(Stato,A,D),
	append([usura(0),pitstop(N1),in(p(S1,T1)),giro(G1)],A,Add),
	append([usura(Q),pitstop(N),in(p(S0,T0)),giro(G)],D,Del).
add_del(taglia_traguardo,Stato,[in(traguardo)],[in(p(S,T))],0) :-
	member(in(p(S,T)),Stato),
	sez_succ(p(S,T),traguardo),
	member(giro(G),Stato),
	giri(N),
	G =:= N.

%=============================================================================== UTILS

pred(check_usura_stato(p_node,sezione,traiettoria,number,number)).
% come check_usura di mondo_macchina ma controllo usura come fluente dello stato
check_usura_stato(Stato,S,T,Q,Q1) :-
	member(usura(Q),Stato),
	calc_usura(S,T,Q,Q1),
	usura_massima(Qmax),
	Q1 =< Qmax.
pred(sposta_avversari_stato(p_node,list,list)).
% sposta_avversari(Stato,A,D): sposta tutti gli avversari (evitando di farli
% 	andare su dove è adesso la macchina o altri avversari) e dà in output la
%	lista degli avversari con le nuove (A) e vecchie (D) posizioni.
% MODO: (+,-,-) semidet.
sposta_avversari_stato(Stato,A,D) :-
	not(member(avversario(_,p(_,_)),Stato)) ->
		A = [],
		D = []
	;
		setof(avversario(Nome,p(S,T)),member(avversario(Nome,p(S,T)),Stato),D),
		maplist(sposta_random(Stato),D,A).

sposta_random(Stato,avversario(Nome,p(S0,T0)),avversario(Nome,P)) :-
	findall(
		p(S1,T1),
		(
			(
				sez_succ(p(S0,T0),p(S1,T1))
				;
				sez_succ(p(S0,T0),traguardo),
				sez_succ(p(S0,T0),p(S1,T1))
			),
			(				
				not(member(in(p(S1,T1)),Stato)),
				not(member(avversario(_,p(S1,T1)),Stato))
			)
		),
		Soluzioni	
	),
	length(Soluzioni,N),
	N > 0,
	random(0,N,I),
	Index is min(N-1,I),
	(
		nth0(Index,Soluzioni,P) %----------------------------------------------- scelta random punto avversario
		;
		true
	).
		

%=============================================================================== EURISTICHE
% i commenti avete l'euristica 0, quella di base sottostimata
% e con la propietà triangolare, quella resa aggressiva moltiplicando
% per 4, non più sottostimata

h(_St,0) :- !.

% h(St,H)	:- capacita(C),
% 	(   setof(sporco(S,Q), (member(sporco(S,Q),St), Q>0), QQ)
% 	;   QQ=[]),!,
% 	sum(QQ,0,H,C).
% 	%sum(QQ,0,H1,C),
%         %H is 4*H1.

% sum([],Sum,Sum,_).
% sum([Q|QQ],Sum1,Sum2,C) :-
% 	sum_stanza(Q,Sum1,Sum,C),
% 	sum(QQ,Sum,Sum2,C).

% sum_stanza(sporco(S,Q), Sum1, Sum2,C) :-
% 	distanza_balcone(S,D),
% 	Sum2 is Sum1 + Q + D*(Q div C + 1).

%=============================================================================== PIANIFICAZIONE E START-GOAL

local_pred(stato_iniziale(decisione_complessa,list(fluent))).
% stato_iniziale(D,S): S è l'insieme ordinato di fluenti che corriponde allo
% 	stato del mondo nel momento in cui la macchina pianifica la decisione D:

local_pred(stato_goal(decisione_complessa,list(fluent))).
% stato_goal(D,S): S è l'insieme ordinato di fluenti che corriponde allo stato
%	goal nel quale non vi sono giri rimasti da fare, la macchina è arrivata al
%	traguardo e non ha mai avuto un usura maggiore dell'usura massima.
% NOTA: si tratta di uno stato di pianificazione, non dello stato
% 	attuale della macchina

pred(piano(decisione_complessa,list(action),number)).
% piano(Dec,Plan,C): pianifica piano di costo C che attua la decisione Dec
% MODO (++,--,--) nondet

stato_iniziale(gareggia,Stato) :-
	(
		not(avversario(_,p(_,_))) ->
		ListaAvversari = [];
		setof(avversario(Nome,p(S,T)),avversario(Nome,p(S,T)),ListaAvversari)
	),
	(in(L); L = griglia),
	(usura(Q); Q = 0),
	(giro(G); G = 0),
	(pitstop(N); N = 0),
	maplist(write, ['\n\nSTATO INIZIALE: ', 'in=', L, ', usura=', Q, ', giro=', G, ', pitstop=', N, '\nAVVERSARI: ']),	
	writeln(ListaAvversari),
	list_to_ord_set([in(L),usura(Q),giro(G),pitstop(N)|ListaAvversari],Stato).

stato_goal(Stato) :-
	member(in(traguardo),Stato),
	member(usura(Q),Stato),
	usura_massima(Qmax),
	Q =< Qmax,
	member(giro(G),Stato),
	giri(N),
	G =:= N,

	setof(avversario(Nome,p(S,T)),member(avversario(Nome,p(S,T)),Stato),D),
	maplist(write, ['\n\nAVVERSARI: ', D]),
	maplist(write, ['\nUSURA: ', Q, '/', Qmax, '\n\n']).

% Uso i predicati stato_iniziale/2 e stato_goal/1 per passare stato iniziale e
% stato goal al planner
piano(D,Plan,Cost) :-
	plan(stato_iniziale(D),
	     stato_goal,
			     [_Sin|_Path],
			     _Goal,
			     Cost,
				 Plan).

%===============================================================================   TEST EURISTICHE

test(K, Cost, Plan) :-
	plan(stato_iniziale(K), stato_goal, _Path,_,Plan, Cost).

stato_iniziale(1, S) :-
   consult(mondi/mondo1),
   list_to_ord_set([in(griglia),usura(0),pitstop(0),giro(0)],S).
stato_iniziale(2, S) :-
    consult(mondi/mondo1),
   list_to_ord_set([in(balcone),raccolto(0),
		    sporco(a,66), sporco(b,30), sporco(c,35), sporco(d,50)], S).

stato_iniziale(3, S) :-
    consult(mondi/mondo2),
   list_to_ord_set([in(balcone),raccolto(0),
		    sporco(a,66), sporco(b,30), sporco(c,35), sporco(d,40),sporco(e,20)], S).



