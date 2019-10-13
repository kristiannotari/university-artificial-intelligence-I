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
type(open:velocita).
type(p(sezione,traiettoria):punto).
type([punto,box]:luogo).

open_pred(pitlane_costo(number)).
% pitlane_costo(QU): costo di attraversamento della pitlane
% MODO (-) det.
open_pred(pitlane_in(sezione,traiettoria)).
% pitlane_in(S,T): dal punto(S,T) si può accedere ai pit
% MODO (-,-) det.
open_pred(pitlane_out(sezione,traiettoria)).
% pitlane_out(S,T): uscendo dai pit si passa alla sezione S in traiettoria T
% MODO (-,-) det.
open_pred(griglia(sezione,traiettoria)).
% griglia(S,T): in partenza la macchina si trova nella sezione S in traiettoria T
% MODO (-,-) det.
open_pred(traguardo(sezione)).
% traguardo(S): S è una sezione con traguardo
% MODO (-) det.
open_pred(costo(velocita,sezione,traiettoria,number,number)).
% costo(V,S,T,QU,QT): data una velocità di percorrenza V, una sezione S e una traiettoria T restituisce il costo in usura (QU) e di tempo (QT)
% MODO (+,+,+,-,-) det.
open_pred(giri(number)).
% giri(N): N numero di giri da effettuare
% MODO (-) det.
open_pred(sez_succ(punto,punto)).
% sez_succ(S1, S2): S2 è sezione successiva di S1 se:
% 	- S1 precede strettamente S2 nel tracciato
%	- S2 è l'ultima sezione del tracciato e S1 è la prima (tracciato chiuso)
% MODO (+,-) det.

open_pred(usura_massima(number)).
% usura_massima(QU): QU quantità massima di usura degli pneumatici della macchina
% MODO (-) det.
open_pred(velocita(velocita)).
% velocita(V): V velocità disponibile per la macchina
% MODO (+) det.
open_pred(usura_massima_velocita(velocita,number)).
% usura_massima(V,QU): QU quantità massima di usura degli pneumatici della macchina per poter andare alla velocità V
% MODO (+,-) det.

%===============================================================================  MONDO, PARTE DINAMICA

pred(in(luogo)).
% in(L): la macchina si trova nel luogo L
% MODO (?) semidet.
:- dynamic(in/1).
pred(usura(number)).
% usura(QU): QU quantità di usura degli pneumatici
% MODO (?) semidet.
:- dynamic(usura/1).
pred(tempo(number)).
% tempo(QT): QT quantità di tempo usato
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

%===============================================================================  ESECUZIONE AZIONI NEL MONDO

type([schierati, guida(punto,velocita), effettua_pitstop, taglia_traguardo]:action).
% le azioni
type([schierato, partito(velocita), spostato(punto,velocita), fermato_ai_pit(punto), fine_giro(velocita), arrivato]:cambiamento).
% schierato:
%	la macchina dai box viene schierata in griglia pronta a partire
% partito(V):
%	la macchina dalla griglia guida verso la sua prima sezione con velocità V
% spostato(p(S,T),V):
%	la macchina passa in un nuovo punto alla velocità V, informando del suo vecchio punto dato da
% 	S e T
% fermato_ai_pit(p(S,T)): 
%	la macchina esegue il pitstop ai pit, entrandovi dal punto(S,T)
% fine_giro:
%	la macchina completa un giro passando dalla sezione del tracciato a quella
%	successiva
% arrivato:
%	la macchina termina la gara al traguardo e va ai box

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
	retractall(tempo(_)),
	retractall(pitstop(_)),
	retractall(giro(_)),
	nb_setval(step,0),
	assert(in(box)),
	consult(mondi/mondo1a). % --------------------------------------------------- caricamento del mondo scelto
:- clear_db.

esecuzione(schierati) :-
	in(box),
	clear_db,
	griglia(S,T),
	change(
		[in(box)],
		[in(p(S,T)),usura(0),tempo(0),pitstop(0),giro(0)],
		schierato
	).
esecuzione(guida(p(S1,T1),V)) :-
	in(p(S0,T0)),
	sez_succ(S0,S1),
	check_movimento(V,S1,T1,QU0,QU1,QT0,QT1),
	giro(G),
	R0 = [in(p(S0,T0)),usura(QU0),tempo(QT0)],
	A0 = [in(p(S1,T1)),usura(QU1),tempo(QT1)],
	(
		caso_speciale_guida(G,V,p(S0,T0),R1,A1,C),
		append(R0,R1,R),
		append(A0,A1,A)
		;
		(
			maybe(0.1),
			writeln("PUNTO OCCUPATOOOOOOOOO"),
			throw(punto_occupato(G,p(S1,T1))) %--------------------------- throw punto occupato
			;
			writeln("PUNTO LIBERO"),
			R = R0,
			A = A0,
			C = spostato(p(S0,T0),V)
		)
	),
	change(R,A,C).
esecuzione(effettua_pitstop) :-
	in(p(S0,T0)),
	pitlane_in(S0,T0),
	pitlane_costo(QT),
	tempo(QT0),
	(
		% se ho completato i giri
		ultimo_giro ->
		(
			QT1 is QT0 + (QT / 2), % se termino la gara ai pit perdo il tempo del pit (ma non tutto quanto)
			% -> termino la gara
			change(
				[tempo(QT0),in(p(S0,T0))],
				[tempo(QT1),in(box)],
				arrivato
			)
		)
		;
		(
			% ; altrimenti effettuo pitstop
			pitlane_out(S1,T1),
			usura(QU0),
			pitstop(P),
			giro(G),
			P1 is P + 1,
			G1 is G + 1,
			QT1 is QT0 + QT,
			change(
				[usura(QU0),tempo(QT0),pitstop(P),in(p(S0,T0)),giro(G)],
				[usura(0),tempo(QT1),pitstop(P1),in(p(S1,T1)),giro(G1)],
				fermato_ai_pit(p(S0,T0))
			)
		)
	).
esecuzione(taglia_traguardo) :-
	in(p(S,T)),
	traguardo(S),
	ultimo_giro,
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

pred(check_movimento(sezione,traiettoria,number,number)).
% check_movimento(V,S,T,QU0,QU1,QT0,QT1): data una velocità V, una sezione S e una traiettoria T, restituisce la
%	usura attuale QU0 e l'usura futura QU1, controllando che non si superi la
%	massima usura e il tempo attuale QT0 e il tempo futuro QT1, controllando che si possa usare la velocità V
% MODO: (+,+,+,-,-,-,-) det.
check_movimento(V,S,T,QU0,QU1,QT0,QT1) :-
	usura(QU0),
	tempo(QT0),
	costo(V,S,T,QU,QT),
	QU1 is QU0 + QU,
	QT1 is QT0 + QT,
	usura_massima(Qmax),
	QU1 =< Qmax,
	usura_massima_velocita(V,QmaxV),
	QU1 =< QmaxV.

pred(caso_speciale_guida(number,velocita,punto,list,list,cambiamento)).
% caso_speciale_guida(G,V,P,R,A,C): dove G è il numero di giri attuale, V la velocità di spostamento attuale, P il punto
% 	da cui la macchina si muove, R e A le liste di retract e assert, mentre C è
%	il cambiamento da mostrare.
% MODO: (+,+,+,-,-,-) det.
% caso speciale guida "partenza da griglia"
caso_speciale_guida(0,V,p(S0,T0),[giro(0)],[giro(1)],partito(V)) :-
	griglia(S0,T0).
% caso speciale guida "taglio del traguardo per nuovo giro"
caso_speciale_guida(G0,V,p(S0,_),[giro(G0)],[giro(G1)],fine_giro(V)) :-
	G0 =\= 0,
	traguardo(S0),
	G1 is G0 + 1.

pred(ultimo_giro).
% ultimo_giro: verifica che sia l'ultimo giro (numero di giri attuale = numero 
%	di giri da effettuare)
% MODO: det.
ultimo_giro :-
	giro(G),
	giri(N),
	G =:= N.

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

% 1 giro
test_case(1, [
	schierati,
	guida(p(san_donato,centrale),v2),
	guida(p(luco,centrale),v2),
	guida(p(poggio_secco,interna),v2),
	guida(p(1,centrale),v2),
	guida(p(bucine,interna),v2),
	guida(p(rettifilo,interna),v2),
	taglia_traguardo
]).
% 2 giri + pitstop
test_case(2, [
	schierati,
	guida(p(san_donato,centrale),v2),
	guida(p(luco,centrale),v2),
	guida(p(poggio_secco,interna),v2),
	guida(p(1,centrale),v2),
	guida(p(bucine,esterna),v2),
	effettua_pitstop,
	guida(p(luco,centrale),v2),
	guida(p(poggio_secco,interna),v2),
	guida(p(1,centrale),v2),
	guida(p(bucine,interna),v2),
	guida(p(rettifilo,interna),v2),
	taglia_traguardo
]).
% 2 giri no pitstop (false con usura_massima 25)
test_case(3, [
	schierati,
	guida(p(san_donato,centrale),v2),
	guida(p(luco,centrale),v2),
	guida(p(poggio_secco,interna),v2),
	guida(p(1,centrale),v2),
	guida(p(bucine,esterna),v2),
	guida(p(rettifilo,interna),v2),
	guida(p(san_donato,centrale),v2),
	guida(p(luco,centrale),v2),
	guida(p(poggio_secco,interna),v2),
	guida(p(1,centrale),v2),
	guida(p(bucine,interna),v2),
	guida(p(rettifilo,interna),v2),
	taglia_traguardo
]).
% 1 giro, v1 fino a che posso, poi false (usura_massima = 25, usura_massima_v1 = 10) (dopo il 3° guida)
test_case(4, [
	schierati,
	guida(p(san_donato,centrale),v1),
	guida(p(luco,centrale),v1),
	guida(p(poggio_secco,interna),v1),
	guida(p(1,centrale),v1),
	guida(p(bucine,interna),v1),
	guida(p(rettifilo,interna),v1),
	taglia_traguardo
]).
% 1 giro, v1 fino a che posso, poi v2
test_case(5, [
		schierati,
		guida(p(san_donato,centrale),v1),
		guida(p(luco,centrale),v1),
		guida(p(poggio_secco,interna),v1),
		guida(p(1,centrale),v2),
		guida(p(bucine,interna),v2),
		guida(p(rettifilo,interna),v2),
		taglia_traguardo
	]).