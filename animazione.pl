:- consult('rrs/util').
:- multifile([type/1, pred/1, local_pred/1, open_pred/1, skipped/1, user_unit/1]).

user_unit(animazione).
%  unità di animazione in senso lato:
%  "visualizza" il comporamento della macchina nel mondo virtuale
%  QUI:  mostra il coportamento passo-passo a livello testuale
%  Ad ogni passo l'utente prosegue con ENTER
%  L'utente può anche abortire con a o chiedere di fare la trace con t
%
%  IMPLEMENTA:
%  mostra(cambiamento).
%  con type([schieramento, partito(punto), spostamento(punto,punto), pitin, pistop, pitout, si_ferma]:cambiamento).
%  così come specificati in mondo_macchina.pl

mostra(schierato) :- !,
	step,
	writeln('schieramento:'),
	write('\tmacchina(griglia)'),
	forall(avversario(p(S,T)), (write(',\n\t'), write(avversario(S,T)))).
mostra(partito(p(S,T))) :- !,
	step,
	giri(N),
	maplist(write, ['partita la gara (giro 1/', N, ')\n\tparto e vado in ', S, '[', T , ']']).
mostra(spostamento(p(S1,T1),p(S2,T2))) :- !,
	step,
	maplist(write, ['passo da ', S1, '[', T1 , '] a ', S2, '[', T2, ']']).
mostra(pitin) :- !,
	step,
	write('entro ai pit').
mostra(pitstop) :- !,
	step,
	write('eseguo pitstop (usura pneumatici torna a 0)').
mostra(pitout(p(S,T))) :- !,
	step,
	maplist(write, ['esco dai pit e ritorno in ', S, '[', T , ']']).
mostra(giro(G)) :- !,
	step,
	giri(N),
	G0 is G - 1,
	maplist(write, ['completato il giro ', G0, '/', N]).
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
