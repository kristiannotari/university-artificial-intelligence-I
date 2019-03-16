:- consult('rrs/util').
:- multifile([type/1, pred/1, local_pred/1, open_pred/1, skipped/1, user_unit/1]).

user_unit(animazione).
% unità di animazione in senso lato:
% "visualizza" il comporamento della macchina nel mondo virtuale
% QUI:  mostra il coportamento passo-passo a livello testuale
% Ad ogni passo l'utente prosegue con ENTER
% L'utente può anche abortire con a o chiedere di fare la trace con t
%
% IMPLEMENTA:
% mostra(cambiamento).
% con type([schieramento, partito(punto), spostamento(punto,punto), pitin, pistop, pitout, si_ferma]:cambiamento).
% così come specificati in mondo_macchina.pl

mostra(schierato) :- !,
	step,
	writeln('schieramento:'),
	write('\tmacchina(griglia)'),
	forall(avversario(Nome,p(S,T)), (write(',\n\t'), write(avversario(Nome,S,T)))).
mostra(partito(p(S,T),Q)) :- !,
	step,
	giri(N),
	maplist(write, ['iniziata la gara (giro 1/', N, ')\n\tparto e vado in ', S, '[', T , '] (usura=', Q, ')']).
mostra(spostamento(p(S1,T1),p(S2,T2),Q)) :- !,
	step,
	maplist(write, ['guido da ', S1, '[', T1 , '] a ', S2, '[', T2, '] (usura=', Q, ')']).
mostra(fermato_ai_pit(G,N,p(S,T))) :- !,
	step,
	maplist(write, ['eseguo pitstop (usura pneumatici torna a 0, pitstop effettuati=', N, ')\n\tesco dai pit in ', S, '[', T , '] e inizio il ', G, '° giro']),
	forall(avversario(Nome,p(S1,T1)), (write(',\n\t'), write(avversario(Nome,S1,T1)))).
mostra(giro(G)) :- !,
	step,
	giri(N),
	G0 is G - 1,
	maplist(write, ['completato il giro ', G0, '/', N]),
	forall(avversario(Nome,p(S,T)), (write(',\n\t'), write(avversario(Nome,S,T)))).
mostra(arrivato) :- !,
	step,
	giri(N),
	maplist(write, ['arrivato al traguardo dopo ', N, ' giri']).

mostra(Bo) :-
	step,
	writeln('[cambiamento non riconosciuto]':Bo).

skipped(step/0).
step :-
	catch(nb_getval(step, NPrec),_,NPrec=0),
	read_command(['\n', NPrec, ': ']),
	N is NPrec+1,
	nb_setval(step,N).
