:- consult(animazione).
:- multifile([type/1, pred/1, local_pred/1, open_pred/1, skipped/1, user_unit/1]).

%===============================================================================  MONDO, PARTE STATICA

user_unit(mondo_macchina).
% Unità che fornisce il mondo costituito dal tracciato, con le sue sezioni e
% traiettorie e dalle macchine e le loro azioni e contiene:
%	- un modello virtuale del mondo esterno (parte statica in un mondo nella
%	cartella "mondi" e parte dinamica in mondo_macchina.pl)
% 	- un'implementazione delle azioni nel mondo virtuale (predicati esecuzione
%	e mostra)

type(open:sezione).
type(open:traiettoria).
type(p(sezione,traiettoria):punto).
type([griglia,traguardo,pit,punto]:luogo).

open_pred(tracciato(list)).
% tracciato(L): lista ordinata delle sezioni del tracciato
open_pred(pitlane_in(punto)).
% pitlane_in(P): dal punto P si può accedere ai pit
open_pred(pitlane_out(punto)).
% pitlane_out(p(S,T)): uscendo dai pit si passa alla sezione S in traiettoria T
open_pred(traiettoria(traiettoria)).
% traiettoria(T): T è una traiettoria
open_pred(cambio(traiettoria,traiettoria)).
% cambio(T1,T2): se è possibile lo spostamento da T1 a T2
open_pred(calc_usura(sezione,traiettoria,number,number)).
% calc_usura(S,T,Q,Q1): data una sezione S, una traiettoria T e un'usura attuale
% Q, calcola l'usura futura Q1
% MODO (+,+,+,-) nondet.
open_pred(giri(number)).
% giri(N): N numero di giri da effettuare
% MODO (?) semidet.

pred(sez_succ(luogo,luogo)).
% sez_succ(L1, L2): L2 è il luogo successivo a L1 se:
%	- L1 (o L2) è un pit e L2 (o L1) è una sezione S dove c'è la pitlane
%	- se L1 e L2 sono sezioni una dopo l'altra del tracciato
%	- se L1 è la griglia e L2 è la prima sezione del tracciato
%	- se L2 è il traguardo e L1 è l'ultima sezione del tracciato
% MODO (?,?) nondet.
sez_succ(p(S,T),pit) :-
	pitlane_in(p(S,T)).
sez_succ(pit,p(S,T)) :-
	pitlane_out(p(S,T)).
sez_succ(p(S1,T1),p(S2,T2)) :-
	tracciato(SS),
	append(_, [S1|[S2|_]], SS),
	traiettoria(T1),
	traiettoria(T2),
	cambio(T1,T2).
sez_succ(griglia,p(S,T)) :-
	traiettoria(T),
	tracciato([S|_]).
sez_succ(p(S,T),traguardo) :-
	traiettoria(T),
	tracciato(SS),
	append(_,[S],SS).

pred(usura_massima(number)).
% usura_massima(Q): Q quantità massima di usura degli pneumatici della macchina
% MODO (?) semidet.
usura_massima(30).

%===============================================================================  MONDO, PARTE DINAMICA

pred(in(luogo)).
% in(L) : la macchina si trova nel luogo L
% MODO  (?) semidet.
:- dynamic(in/1).
pred(usura(number)).
% usura(Q) : Q quantità di usura degli pneumatici
% MODO (?) semidet.
:- dynamic(usura/1).
pred(pitstop(number)).
% pitstop(N) : N numero di pitstop effettuati
% MODO (?) semidet.
:- dynamic(usura/1).
pred(giro(number)).
% giro(N) : N numero del giro in corso
% MODO (?) semidet.
:- dynamic(giro/1).
open_pred(avversario(atom,punto)).
% avversario(N, p(S,T)): c'è un avversario (N) nella sezione S in traiettoria T
% MODO (?) semidet.
:- dynamic(avversario/2).

%===============================================================================  ESECUZIONE AZIONI NEL MONDO

type([schierati, guida(punto), effettua_pitstop, taglia_traguardo]:action).
% le azioni
type([schierato, partito(punto,number), spostamento(punto,punto,number), fermato_ai_pit(number,number,punto), giro, arrivato]:cambiamento).
% schierato:
%	la macchina viene schierata in griglia pronta a partire
% partito(p(S,T),Q):
%	la macchina dalla griglia passa alla sezione S in traiettoria T, Q è la
%	nuova usura
% spostamento(p(S1,T1),p(S2,T2),Q):
%	la macchina passa alla sezione S1 in traiettoria T1 alla sezione S2 in
%	traiettoria T2, e Q è la nuova usura
% fermato_ai_pit(G,N,p(S,T)): 
%	la macchina esegue il pitstop ai pit, effettuati N pitstop nella gara, e 
%	rientra nella sezione S in traiettoria T nel corso del Gesimo giro
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
	retractall(pitstop(_)),
	retractall(giro(_)),
	retractall(avversario(_,_)),
	nb_setval(step,0),
	consult(mondi/mondo1). % --------------------------------------------------- caricamento del mondo scelto
:- clear_db.

esecuzione(schierati) :-
	clear_db,
	change(
		[],
		[in(griglia),usura(0),pitstop(0),giro(0)],
		schierato
	).
esecuzione(guida(p(S,T))) :-
	in(L),
	(
		sez_succ(L,traguardo),
		From = griglia
		;
		From = L
	),
	check_guidabilita(From,p(S,T)),
	check_usura(S,T,Q,Q1),
	(
		R0 = [in(L),usura(Q)],
		A0 = [in(p(S,T)),usura(Q1)],
		(
			From = griglia ->  
			(
				giro(G),
				G1 is G + 1,
				R = [giro(G)|R0],
				A = [giro(G1)|A0],
				(
					G = 0 ->
					C = partito(p(S,T),Q1)
					;
					sposta_avversari,
					C = giro(G1)
				)
			)
			;
			(
				R = R0,
				A = A0,
				C = spostamento(L,p(S,T),Q1)
			)
		),
		change(R,A,C)
	).
esecuzione(effettua_pitstop) :-
	in(p(S0,T0)),
	sez_succ(p(S0,T0),pit),
	check_guidabilita(pit,p(S1,T1)),
	usura(Q),
	giro(G),
	pitstop(N),
	N1 is N + 1,
	G1 is G + 1,
	sposta_avversari,
	change(
		[usura(Q),pitstop(N),in(p(S0,T0)),giro(G)],
		[usura(0),pitstop(N1),in(p(S1,T1)),giro(G1)],
		fermato_ai_pit(G1,N1,p(S1,T1))
	).
esecuzione(taglia_traguardo) :-
	in(p(S,T)),
	sez_succ(p(S,T),traguardo),
	giro(G),
	giri(N),
	G =:= N,
	change(
		[in(p(S,T))],
		[in(traguardo)],
		arrivato
	).	

%===============================================================================  UTILS

pred(change(list,list,cambiamento)).
% change(R,A,C): effettua la transizione di stato facendo il retract su R, lo
%	assert su A e mostrando il cambiamento C
% MODO: (?,?,?) nondet.
change(R,A,C) :-
	maplist(retract, R),
	maplist(assert, A),
	mostra(C).

pred(check_usura(sezione,traiettoria,number,number)).
% check_usura(S,T,Q,Q1): data una sezione S e una traiettoria T, restituisce la
%	usura attuale Q e l'usura futura Q1, controllando che non si superi la
%	massima usura
% MODO: (+,+,-,-) semidet.
check_usura(S,T,Q,Q1) :-
	usura(Q),
	calc_usura(S,T,Q,Q1),
	usura_massima(Qmax),
	Q1 =< Qmax.

pred(check_guidabilita(luogo,luogo)).
% check_guidabilita(L0,L1): verifica che la macchina possa muoversi nella
% 	sezione successiva a quella in input L0 (anche con controllo avversari)
% MODO: (+,?) semidet.
check_guidabilita(L0,L1) :-
	sez_succ(L0,L1),
	(
		not(avversario(_,L1)), !
		;
		avversario(_,L1),
		throw(punto_occupato_macchina(L0,L1)) %--------------------------------- throw punto occupato macchina
	).

pred(sposta_avversari).
% sposta_avversari: sposta tutti gli avversari (evitando di farli andare
%	su dove è adesso la macchina o un altro avversario)
% MODO: (+) semidet.
sposta_avversari :-
	not(avversario(_,p(_,_)))
	;
	forall(
		avversario(Nome,p(S0,T0)),
		(
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
						not(in(p(S1,T1))),
						not(avversario(_,p(S1,T1)))
					)
				),
				Soluzioni
			),
			length(Soluzioni,N),
			(
				N > 0
				;
				throw(punto_occupato_avversario(Nome)) %------------------------ throw punto occupato avversario				
			),
			member(P,Soluzioni),
			retract(avversario(Nome,p(S0,T0))),
			assert(avversario(Nome,P))
		)
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

test_case(1, [
	schierati,
	guida(p(san_donato,centrale)),
	guida(p(luco,centrale)),
	guida(p(1,centrale)),
	guida(p(poggio_secco,interna)),
	guida(p(2,interna)),
	taglia_traguardo
]).
test_case(2, [
	schierati,
	guida(p(san_donato,centrale)),
	guida(p(luco,centrale)),
	guida(p(1,centrale)),
	guida(p(poggio_secco,interna)),
	guida(p(2,interna)),
	effettua_pitstop,
	guida(p(luco,centrale)),
	guida(p(1,esterna)),
	guida(p(poggio_secco,centrale)),
	guida(p(2,interna)),
	taglia_traguardo
]).
test_case(3, [
	schierati,
	guida(p(san_donato,centrale)),
	guida(p(luco,interna)),
	guida(p(1,centrale)),
	guida(p(poggio_secco,interna)),
	guida(p(2,interna)),
	effettua_pitstop,
	guida(p(luco,interna)),
	guida(p(1,centrale)),
	guida(p(poggio_secco,interna)),
	guida(p(2,interna)),
	guida(p(san_donato,interna)),
	guida(p(luco,interna)),
	guida(p(1,centrale)),
	guida(p(poggio_secco,interna)),
	guida(p(2,interna)),
	taglia_traguardo
]).
% test di azioni pianificate
test_case(4, [
	schierati,
	guida(p(san_donato, interna)),
	guida(p(luco, interna)),
	guida(p(1, interna)),
	guida(p(poggio_secco, interna)),
	guida(p(2, centrale)),
	guida(p(san_donato,centrale)),
	guida(p(luco, interna)),
	guida(p(1, interna)),
	guida(p(poggio_secco, interna)),
	guida(p(2, interna)),
	taglia_traguardo
]).	  