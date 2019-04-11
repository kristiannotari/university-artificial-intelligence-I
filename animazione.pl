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
% con type([schierato, partito, spostato(punto), fermato_ai_pit(punto), giro, arrivato]:cambiamento).
% così come specificati in mondo_macchina.pl

mostra(schierato) :- !,
	step,
	writeln("schieramento:"),
	write("\tmacchina(griglia)"),
	forall(avversario(Nome,S,T), (write(",\n\t"), write(avversario(Nome,S,T)))).
mostra(partito) :- !,
	step,
	usura(Q),
	in(p(S,T)),
	giri(N),
	griglia(S0,T0),
	maplist(write, ["iniziata la gara (giro 1/", N, ")\n\tparto da ", S0, "[", T0, "] e vado in ", S, "[", T , "] (usura=", Q, ")"]).
mostra(spostato(p(S0,T0))) :- !,
	step,
	usura(Q),
	in(p(S1,T1)),
	maplist(write, ["guido da ", S0, "[", T0 , "] a ", S1, "[", T1, "] (usura=", Q, ")"]).
mostra(fermato_ai_pit(p(S0,T0))) :- !,
	step,
	in(p(S1,T1)),
	giro(G),
	pitstop(P),
	maplist(write, ["entro ai pit da ", S0, "[", T0, "], eseguo pitstop (usura pneumatici torna a 0, pitstop effettuati=", P, ")\n\tesco dai pit in ", S1, "[", T1 , "] e inizio il ", G, "° giro"]),
	forall(avversario(Nome,S,T), (write(",\n\t"), write(avversario(Nome,S,T)))).
mostra(giro) :- !,
	step,
	giri(N),
	giro(G),
	G0 is G - 1,
	maplist(write, ["completato il giro ", G0, "/", N, " e guidato in ", S, "[", T, "]"]),
	forall(avversario(Nome,S,T), (write(",\n\t"), write(avversario(Nome,S,T)))).
mostra(arrivato) :- !,
	step,
	giri(N),
	maplist(write, ["arrivato al traguardo dopo ", N, " giri"]).

mostra(Bo) :-
	step,
	writeln('[cambiamento non riconosciuto]':Bo).

skipped(step/0).
step :-
	catch(nb_getval(step, NPrec),_,NPrec=0),
	read_command(['\n', NPrec, ': ']),
	N is NPrec+1,
	nb_setval(step,N).
