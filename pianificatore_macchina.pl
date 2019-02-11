:- consult('rrs/forward_planner').
:- consult('rrs/strips').
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
% implemenda forward_planner:h

local_pred(sum(list(fluent),number,number,number)).
% sum(Fluents, N1, N2, C) :
% C è la capacità dell'aspirapolvere.
% per ogni sporco(S,Q) in Fluents, somma a N1:
% Q + distanza_balcone(S)*(Q div C + 1)
% cioè la quantità Q di sporco nella stanza S + il
% numero di viaggi necessari per portare Q in pattumiera
% (dato da Q div C + 1) moltiplicato per la distanza di S dal
% balcone

local_pred(sum_stanza(fluent, number, number, number)).
%   sum_stanza(sporco(S,Q),N1,N2,C) : C � la capacit�
%   dell'aspirapolvere; N2 = N1 + Q + distanza_balcone(S)*(Q div C + 1)

%pred(add_del(action, p_node, list(fluent), list(fluent), number)).
%   IMPLEMENTA strips:add_del

add_del(pulisci(Q1), St,  [sporco(S,Q2),raccolto(R1)], [sporco(S,Q),raccolto(R)], Q) :-
	member(in(S), St),
	member(sporco(S,Q), St),
	Q > 0,
	capacita(C),
	member(raccolto(R),St),
	R < C,
	Q1 is min(Q, C-R),
	Q2 is Q-Q1,
	R1 is R+Q1.
add_del(va(S), St, [in(S)], [in(SPrec)], 1) :-
	member(in(SPrec), St),
	comunica(SPrec,S)
	,
	(   SPrec = balcone
	;   member(raccolto(R), St),
	    capacita(C),
	    member(sporco(SPrec,Q), St),
	    not((Q>0,R<C))
	).
add_del(versa_sporco, St,  [raccolto(0)], [raccolto(R)], R) :-
	member(in(balcone), St),
	member(raccolto(R), St).


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

local_pred(stato_iniziale(list(fluent))).
% stato_iniziale(S): S è l'insieme ordinato di fluenti che corriponde allo
% 	stato del mondo nel momento in cui la macchina pianifica

local_pred(stato_goal(list(fluent))).
% stato_goal(S): S è l'insieme ordinato di fluenti che corriponde allo stato
%	goal, nel quale non vi sono giri rimasti da fare, la macchina è arrivata al
%	traguardo e non ha mai avuto un usura maggiore dell'usura massima.
% NOTA: si tratta di uno stato di pianificazione, non dello stato
% 	attuale della macchina

pred(piano(decisione_complessa, list(action), number)).
% piano(Dec,Plan,C): pianifica piano di costo C che attua la decisione Dec
% MODO (++,--,--) nondet

stato_iniziale(St) :-
	setof(sporco(S,Q), sporco(S,Q) , ListSporco),
	in(Stanza),
	raccolto(R),
	list_to_ord_set([in(Stanza),raccolto(R)|ListSporco], St).
stato_goal(S) :-
	member(in(balcone),S),
	member(raccolto(0),S),
	not((member(sporco(_,Q),S), Q > 0)), !.

% Uso i predicati stato_iniziale/1 e stato_goal/1 per passare stato iniziale e
% stato goal al planner
piano(esegui_pulizia, Plan, Cost) :-
	plan(stato_iniziale,
	     stato_goal,
			     [_Sin|_Path],
			     _Goal,
			     Cost,
			     Plan).

%===============================================================================   TEST EURISTICHE

test(K, Cost, Plan) :-
	plan(stato_iniziale(K), stato_goal, _Path,_,Plan, Cost).

stato_iniziale(1, S) :-
   consult(mondo1),
   list_to_ord_set([in(balcone),raccolto(0),
		    sporco(a,16), sporco(b,20), sporco(c,15), sporco(d,5)], S).
stato_iniziale(2, S) :-
    consult(mondo1),
   list_to_ord_set([in(balcone),raccolto(0),
		    sporco(a,66), sporco(b,30), sporco(c,35), sporco(d,50)], S).

stato_iniziale(3, S) :-
    consult(mondo2),
   list_to_ord_set([in(balcone),raccolto(0),
		    sporco(a,66), sporco(b,30), sporco(c,35), sporco(d,40),sporco(e,20)], S).



