:- consult('rrs/util').
:- multifile([type/1, pred/1, local_pred/1, open_pred/1, skipped/1, user_unit/1]).

user_unit(animazione).
% unità di animazione in senso lato:
% "visualizza" il comportamento della macchina nel mondo virtuale
% QUI: mostra il comportamento passo-passo a livello testuale
% Ad ogni passo l'utente prosegue con ENTER
% L'utente può anche abortire con a o chiedere di fare la trace con t
%
% IMPLEMENTA:
% mostra(cambiamento).
% con type([schierato, partito(velocita), spostato(punto,velocita), fermato_ai_pit(punto), fine_giro(velocita), arrivato]:cambiamento).
% così come specificati in mondo_macchina.pl

mostra(schierato) :- !,
	step,
	in(p(S,T)),
	maplist(write, ["[SCHIERATO]\nschiero la macchina in ", S, "[", T, "] (griglia)"]).
mostra(partito(V)) :- !,
	step,
	usura(QU),
	tempo(QT),
	in(p(S,T)),
	giri(N),
	maplist(write, ["[PARTITO]\niniziata la gara (giro 1/", N, ")\n\tparto dalla griglia e vado in ", S, "[", T , "]\n\tvelocità=", V, ", usura=", QU, ", tempo=", QT]).
mostra(spostato(p(S0,T0),V)) :- !,
	step,
	usura(QU),
	tempo(QT),
	in(p(S1,T1)),
	maplist(write, ["[SPOSTATO]\nguido da ", S0, "[", T0 , "] a ", S1, "[", T1, "]\n\tvelocità=", V,", usura=", QU, ", tempo=", QT]).
mostra(fermato_ai_pit(p(S0,T0))) :- !,
	step,
	in(p(S1,T1)),
	giro(G),
	pitstop(P),
	maplist(write, ["[FERMATO_AI_PIT]\nentro ai pit da ", S0, "[", T0, "]\n\teseguo pitstop (usura pneumatici torna a 0, pitstop effettuati=", P, ")\n\tesco dai pit in ", S1, "[", T1 , "]\n\tinizio il ", G, "° giro"]).
mostra(fine_giro(V)) :- !,
	step,
	in(p(S,T)),
	giri(N),
	giro(G),
	G0 is G - 1,
	usura(QU),
	tempo(QT),
	maplist(write, ["[FINE_GIRO]\ncompletato il giro ", G0, "/", N, "\n\tguidato in ", S, "[", T, "]\n\tvelocità=", V,", usura=", QU, ", tempo=", QT]).
mostra(arrivato) :- !,
	step,
	giri(N),
	usura(QU),
	tempo(QT),
	maplist(write, ["[ARRIVATO]\narrivato al traguardo dopo ", N, " giri\n\tusura=", QU, ", tempo=", QT ,")"]).
mostra(Bo) :-
	step,
	writeln('[cambiamento non riconosciuto]':Bo).

skipped(step/0).
step :-
	catch(nb_getval(step, NPrec),_,NPrec=0),
	read_command(['\n', NPrec, ': ']),
	N is NPrec+1,
	nb_setval(step,N).
