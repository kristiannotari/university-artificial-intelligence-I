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
type([punto,box]:luogo).

open_pred(tracciato(list)).
% tracciato(L): lista ordinata delle sezioni del tracciato
open_pred(pitlane_in(sezione,traiettoria)).
% pitlane_in(S,T): dal punto(S,T) si può accedere ai pit
open_pred(pitlane_out(sezione,traiettoria)).
% pitlane_out(S,T): uscendo dai pit si passa alla sezione S in traiettoria T
open_pred(griglia(sezione,traiettoria)).
% griglia(S,T): in partenza la macchina si trova nella sezione S in traiettoria T
open_pred(traguardo(sezione)).
% traguardo(S): S è una sezione con traguardo
open_pred(costo(sezione,traiettoria,number)).
% costo(S,T,Q): data una sezione S e una traiettoria T restituisce il costo (in 
% 	usura) di quel punto
% MODO (+,+,-) det.
open_pred(giri(number)).
% giri(N): N numero di giri da effettuare
% MODO (?) semidet.

pred(sez_succ(punto,punto)).
% sez_succ(S1, S2): S2 è sezione successiva di S1 se:
% 	- S1 precede di 1 come posizione nel tracciato S2
%	- S2 è l'ultima sezione del tracciato e S1 è la prima
% MODO (+,-) det.
sez_succ(S1,S2) :-
	tracciato(SS),
	(
		append(_, [S1|[S2|_]], SS)
		;
		SS = [S2|_],
		last(SS,S1)
	).

pred(usura_massima(number)).
% usura_massima(Q): Q quantità massima di usura degli pneumatici della macchina
% MODO (?) semidet.
usura_massima(30).

%===============================================================================  MONDO, PARTE DINAMICA

pred(in(luogo)).
% in(L): la macchina si trova nel luogo L
% MODO (?) semidet.
:- dynamic(in/1).
pred(usura(number)).
% usura(Q): Q quantità di usura degli pneumatici
% MODO (?) semidet.
:- dynamic(usura/1).
pred(pitstop(number)).
% pitstop(N): N numero di pitstop effettuati
% MODO (?) semidet.
:- dynamic(usura/1).
pred(giro(number)).
% giro(N): N numero del giro in corso
% MODO (?) semidet.
:- dynamic(giro/1).
open_pred(avversario(atom,sezione,traiettoria)).
% avversario(N,S,T)): c'è un avversario (N) nella sezione S in traiettoria T
% MODO (?) semidet.
:- dynamic(avversario/3).

%===============================================================================  ESECUZIONE AZIONI NEL MONDO

type([schierati, guida(punto), effettua_pitstop, taglia_traguardo]:action).
% le azioni
type([schierato, partito, spostamento(punto), fermato_ai_pit(punto), giro, arrivato]:cambiamento).
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
	assert(in(box)),
	consult(mondi/mondo1). % --------------------------------------------------- caricamento del mondo scelto
:- clear_db.

esecuzione(schierati) :-
	in(box),
	clear_db,
	griglia(S,T),
	change(
		[in(box)],
		[in(p(S,T)),usura(0),pitstop(0),giro(0)],
		schierato
	).
esecuzione(guida(p(S1,T1))) :-
	in(p(S0,T0)),
	sez_succ(S0,S1),
	check_guidabilita(p(S1,T1)),
	check_usura(S1,T1,Q0,Q1),
	giro(G),
	(
		R0 = [in(p(S0,T0)),usura(Q0)],
		A0 = [in(p(S1,T1)),usura(Q1)],
		(
			caso_speciale_guida(G,p(S0,T0),R1,A1,C),
			append(R0,R1,R),
			append(A0,A1,A)
			;
			R = R0,
			A = A0,
			C = spostamento(p(S0,T0))
		),
		change(R,A,C)
	).
esecuzione(effettua_pitstop) :-
	in(p(S0,T0)),
	pitlane_in(S0,T0),
	pitlane_out(S1,T1),
	check_guidabilita(p(S1,T1)),
	usura(Q),
	giro(G),
	giri(N),
	(
		% se ho completato i giri ->
		G =:= N ->
		(
			% termino la gara
			change(
				[in(p(S0,T0))],
				[in(box)],
				arrivato
			)
		)
		;
		(
			%; altrimenti effettuo pitstop
			pitstop(P),
			P1 is P + 1,
			G1 is G + 1,
			sposta_avversari,
			change(
				[usura(Q),pitstop(P),in(p(S0,T0)),giro(G)],
				[usura(0),pitstop(P1),in(p(S1,T1)),giro(G1)],
				fermato_ai_pit(p(S0,T0))
			)
		)
	).
esecuzione(taglia_traguardo) :-
	in(p(S,T)),
	traguardo(S),
	giro(G),
	giri(N),
	G =:= N,
	change(
		[in(p(S,T))],
		[in(box)],
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
check_usura(S,T,Q0,Q1) :-
	usura(Q0),
	costo(S,T,Q),
	Q1 is Q0 + Q,
	usura_massima(Qmax),
	Q1 =< Qmax.

pred(check_guidabilita(punto)).
% check_guidabilita(p(S,T)): verifica che la macchina possa muoversi nella
% 	sezione S in traiettoria T.
% MODO: (+) semidet.
check_guidabilita(p(S1,T1)) :-
	(
		not(avversario(_,p(S1,T1))), !
		;
		avversario(Nome,p(S1,T1)),
		throw(punto_occupato_macchina(Nome,p(S1,T1))) %--------------------------------- throw punto occupato macchina
	).

pred(caso_speciale_guida(number,punto,list,list,cambiamento)).
% caso_speciale_guida(G,P,R,A,C): dove G è il numero di giri attuale, P il punto
% 	da cui la macchina si muove, R e A le liste di retract e assert, mentre C è
%	il cambiamento da mostrare.
% MODO: (+,-,-,-) semidet.
% caso speciale guida "partenza da griglia"
caso_speciale_guida(0,p(S0,T0),[giro(0)],[giro(1)],partito) :-
	griglia(S0,T0).
% caso speciale guida "taglio del traguardo per nuovo giro"
caso_speciale_guida(G,p(S0,_),[giro(G)],[giro(G1)],giro) :-
	traguardo(S0),
	G1 is G + 1.

pred(sposta_avversari).
% sposta_avversari: sposta tutti gli avversari (evitando di farli andare
%	su dove è adesso la macchina o un altro avversario).
% MODO: nondet.
sposta_avversari :-
	forall(
		avversario(Nome,S0,T0),
		(
			sez_succ(S0,S1),
			setof(
				(Costo,T1),
				T1^(
					costo(S1,T1,Costo),
					not(in(p(S1,T1))),
					not(avversario(_,S1,T1))
				),
				Soluzioni
			),
			length(Soluzioni,N),
			(
				N > 0
				;
				throw(punto_occupato_avversario(Nome)) %------------------------ throw punto occupato avversario				
			),
			(
				maybe(0.9) -> %------------------------------------------------- imprevedibilità avversari (scelta random punto successivo)
					Soluzioni = [(_,T)|_]
					;
					random_member((_,T),Soluzioni)
			),
			retract(avversario(Nome,S0,T0)),
			assert(avversario(Nome,S1,T))
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
	guida(p(poggio_secco,interna)),
	guida(p(1,centrale)),
	guida(p(materassi,interna)),
	guida(p(borgo_san_lorenzo,interna)),
	guida(p(2,interna)),
	guida(p(casanova,interna)),
	guida(p(savelli,interna)),
	guida(p(arrabbiata1,interna)),
	guida(p(arrabbiata2,interna)),
	guida(p(3,interna)),
	guida(p(scarperia,interna)),
	guida(p(palagio,interna)),
	guida(p(4,interna)),
	guida(p(correntaio,interna)),
	guida(p(biondetti1,interna)),
	guida(p(biondetti2,interna)),
	guida(p(5,interna)),
	guida(p(bucine,interna)),
	guida(p(rettifilo,interna)),
	taglia_traguardo
]).
test_case(2, [
		schierati,
		guida(p(san_donato,centrale)),
		guida(p(luco,centrale)),
		guida(p(poggio_secco,interna)),
		guida(p(1,centrale)),
		guida(p(materassi,interna)),
		guida(p(borgo_san_lorenzo,interna)),
		guida(p(2,interna)),
		guida(p(casanova,interna)),
		guida(p(savelli,interna)),
		guida(p(arrabbiata1,interna)),
		guida(p(arrabbiata2,interna)),
		guida(p(3,interna)),
		guida(p(scarperia,interna)),
		guida(p(palagio,interna)),
		guida(p(4,interna)),
		guida(p(correntaio,interna)),
		guida(p(biondetti1,interna)),
		guida(p(biondetti2,interna)),
		guida(p(5,interna)),
		guida(p(bucine,esterna)),
		effettua_pitstop,
		guida(p(luco,centrale)),
		guida(p(poggio_secco,interna)),
		guida(p(1,centrale)),
		guida(p(materassi,interna)),
		guida(p(borgo_san_lorenzo,interna)),
		guida(p(2,interna)),
		guida(p(casanova,interna)),
		guida(p(savelli,interna)),
		guida(p(arrabbiata1,interna)),
		guida(p(arrabbiata2,interna)),
		guida(p(3,interna)),
		guida(p(scarperia,interna)),
		guida(p(palagio,interna)),
		guida(p(4,interna)),
		guida(p(correntaio,interna)),
		guida(p(biondetti1,interna)),
		guida(p(biondetti2,interna)),
		guida(p(5,interna)),
		guida(p(bucine,esterna)),
		guida(p(rettifilo,centrale)),
		taglia_traguardo
	]).
% test di azioni pianificate
test_case(3, [
]).	  