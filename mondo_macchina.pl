:- consult(animazione).
:- multifile([type/1, pred/1, local_pred/1, open_pred/1, skipped/1, user_unit/1]).

%===============================================================================  MONDO, PARTE STATICA

user_unit(mondo_macchina).
% Unità che fornisce il mondo costituito dal tracciato, con le sue sezioni e
% traiettorie e dalle macchine e le loro azioni e ontiene:
%	- un modello virtuale del mondo esterno (parte statica in un mondo nella
%	cartella "mondi" e parte dinamica in mondo_macchina.pl)
% 	- una implementazione delle azioni nel mondo virtuale (predicati esecuzione
%	e mostra)

type(open:curva).
type(open:rettilineo).
type({curva,rettilineo}:sezione).
type(open:traiettoria).
type(p(sezione,traiettoria):punto).
type([griglia,traguardo,pit,punto]:luogo).

open_pred(tracciato(list)).
% tracciato(L): lista ordinata delle sezioni del tracciato
open_pred(pitlane_in(punto)).
% pitlane_in(P): dal punto P si può accedere ai pit
open_pred(pitlane_out(punto)).
% pitlane_out(p(S,T)): uscendo dai pit si passa alla sezione S in traiettoria T
open_pred(avversario(punto)).
% avversario(p(S,T)): c'è un avversario nella sezione S in traiettoria T

pred(succ(luogo,luogo)).
% succ(L1, L2):  L2 è il luogo successivo a L1 se:
%	- L1 (o L2) è un pit e L2 (o L1) è una sezione S dove c'è la pitlane
%	- se L1 e L2 sono sezioni una dopo l'altra del tracciato
%	- se L1 è la griglia e L2 è la prima sezione del tracciato
%	- se L2 è il traguardo e L1 è l'ultima sezione del tracciato
% MODO (?,?) nondet.
succ(p(S,T),pit) :-
	pitlane_in(p(S,T)),
	!.
succ(pit, p(S,T)) :-
	pitlane_out(p(S,T)),
	!.
succ(p(S1,T1),p(S2,T2)) :-
	tracciato(SS),
	append(_, [S1|[S2|_]], SS),
	cambio(T1,T2),
	!.
succ(griglia,p(S,_)) :-
	tracciato([S|_]),
	!.
succ(p(S,_),traguardo) :-
	tracciato(SS),
	append(_,[S],SS),
	!.

open_pred(cambio(traiettoria,traiettoria)).
% cambio(T1,T2): se è possibile lo spostamento da T1 a T2

pred(usura_massima(number)).
% usura_massima(Q): Q quantità massima di usura degli pneumatici della macchina
% MODO (?) semidet.
usura_massima(10).

pred(giri(number)).
% giri(N): N numero di giri da effettuare
% MODO (?) semidet.
giri(2).

:- consult(mondi/mondo1).

%===============================================================================  MONDO, PARTE DINAMICA

pred(in(luogo)).
%  in(L) : la macchina si trova nel luogo L
%  MODO  (?) semidet.
:- dynamic(in/1).
pred(usura(number)).
%  usura(Q) : Q quantità di usura degli pneumatici
%  MODO (?) semidet.
:- dynamic(usura/1).
pred(giro(number)).
%  giro(N) : N numero del giro in corso
%  MODO (?) semidet.
:- dynamic(giro/1).

%===============================================================================  ESECUZIONE AZIONI NEL MONDO

type([schierati, parti(punto), guida(punto), pitin, pitstop, pitout, taglia_traguardo]:action).
%  le azioni
type([schierato, partito(punto), spostamento(punto,punto), pitin, pistop, pitout(punto), giro, arrivato]:cambiamento).
% schierato:
%	la macchina viene schierata in griglia pronta a partire
% partito(p(S,T)):
%	la macchina dalla griglia passa alla sezione S in traiettoria T
% spostamento(p(S1,T1),p(S2,T2)):
%	la macchina passa alla sezione S1 in traiettoria T1 alla sezione S2 in
%	traiettoria T2
% pitin: 
%	la macchina passa dal punto in cui si trova ai pit
% pitstop: 
%	la macchina esegue il pitstop ai pit
% pitout(p(S,T)): 
%	la macchina passa dai pit alla sezione S in traiettoria T
% giro:
%	la macchina completa un giro passando dall'ultima sezione del tracciato al 
%	traguardo
% arrivato:
%	la macchina termina la gara al traguardo

pred(esecuzione(action)).
% esecuzione(A): 
%	effettua l'esecuzione effettiva di A nel mondo virtuale, se A è possibile
%	nello stato corrente.
% MODO  (++) semidet
pred(mostra(cambiamento)).
% mostra(C): mostra il cambiamento nel mondo virtuale (dipende da animazione.pl)

skipped(clear_db/0).
skipped(nb_setval/2).
skipped(nb_getval/2).

clear_db :-
	retractall(in(_)),
	retractall(usura(_)),
	retractall(giro(_)),
	nb_setval(step,0).

esecuzione(schierati) :-
	clear_db,
	assert(in(griglia)),
	assert(usura(0)),
	mostra(schierato).
esecuzione(parti(p(S,T))) :-
	in(griglia),
	succ(griglia,p(S,T)),
	not(avversario(p(S,T))),
	retract(in(griglia)),
	assert(in(p(S,T))),
	assert(giro(1)),
	mostra(partito(p(S,T))).
esecuzione(guida(p(S,T))) :-
	in(p(S0,T0)),
	succ(p(S0,T0),p(S,T)),
	not(avversario(p(S,T))),
	retract(in(p(S0,T0))),
	assert(in(p(S,T))),
	mostra(spostamento(p(S0,T0),p(S,T))).
esecuzione(pitin) :-
	in(p(S,T)),
	succ(p(S,T),pit),
	retract(in(p(S,T))),
	assert(in(pit)),
	mostra(pitin).
esecuzione(pitstop) :-
	in(pit),
	usura(Q),
	retract(usura(Q)),
	assert(usura(0)),
	mostra(pitstop).
esecuzione(pitout) :-
	in(pit),
	succ(pit,p(S,T)),
	retract(in(pit)),
	assert(in(p(S,T))),
	mostra(pitout(p(S,T))).
esecuzione(taglia_traguardo) :-
	in(p(S,T)),
	succ(p(S,T),traguardo),
	retract(in(p(S,T))),
	giro(G),
	giri(N),
	( 
		G =:= N -> assert(in(traguardo)), mostra(arrivato);
		G1 is G+1, 
		succ(griglia,p(S1,T1)),
		retract(giro(G)),
		assert(giro(G1)), 
		assert(in(p(S1,T1))),
		mostra(giro(G1))
	).
		

%===============================================================================  TESTING
	
test_start(K) :-
	test_case(K, Actions),
	writeln("[test] - Premi ENTER per far partire il test [a=abort,t=trace,n=notrace]:"),
	(
		test(Actions);
		nodebug, false
	),
	nodebug.

test([A|L]) :-
	esecuzione(A),
	test(L).
test([]).

test_case(1, [schierati]).
test_case(2, [schierati, parti(p(san_donato,centrale))]).
test_case(3, [schierati, parti(p(san_donato,centrale)), guida(p(luco,interno))]).
test_case(4, [schierati, parti(p(san_donato,centrale)), guida(p(luco,interno)), guida(p(1,esterno))]).
test_case(5, [
	schierati,
	parti(p(san_donato,centrale)),
	guida(p(luco,interna)),
	guida(p(1,centrale)),
	guida(p(poggio_secco,interna)),
	guida(p(2,interna)),
	pitin,
	pitstop,
	pitout,
	taglia_traguardo
]).
