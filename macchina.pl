:- consult(mondo_macchina).
:- consult(pianificatore_macchina).
:- consult(conoscenza_macchina).
:- consult('rrs/decisore_ci').
:- consult('rrs/util').

:- multifile([type/1, pred/1, local_pred/1, open_pred/1, skipped/1, user_unit/1]).

:- maplist(writeln,
['*****************************************************',
 '   Macchina con conoscenza incompleta; macchina_help',
 '*****************************************************']).

macchina_help :- maplist(writeln,
['HELP',
 '?- start. per avviare la macchina',
 ' Traccia di debug attiva per default,',
 ' nodebugtrace per escluderla,  debugtrace per riattivarla\n']).

user_unit(macchina).
% Unità che governa la "vita" della macchina usando
%  l'RRS decisore  (in 'rrs/decisore.pl').
%  Usa (importa) i tipi e i predicati definiti in mondo_macchina
%
% Usa il predicato
%   piano(decisione_complessa, list(action), number)).
%   di pianificatore_aspirapolvere

start :- decisore(ferma).
%  Avvia la macchina

%===============================================================================  STATI INTERNI E INTERRUZIONI MACCHINA

type([punto_occupato(number,punto)]:interrupt_expl).
% Chiudo interrupt_expl; vi sono:
%	- punto_occupato(G,p(S,T)): interruzione quando la macchina non
%	può spostarsi nel punto S,T indicato a causa della presenza di un avversario al giro G
type([ferma,in_gara,interruzione(interrupt_expl),unknown(interrupt)]:state).
% Chiudo il tipo state; la macchina attende di essere fatta partire in stato
% ferma e procede nel circuito nello stato in_gara.
% Quando ha finito il giro, ritorna in ferma.
% Se non può spostarsi in un punto successivo, ripianifica.
type([gareggia]:decisione_complessa).
% Chiudo decisione_complessa con l'unica decisione gareggia
% (fare N giri del circuito senza scontrare altre macchine)

%===============================================================================  EVENTI MACCHINA

type([avvio_gara]:event).
% Evento scatenato dalla partenza della gara
local_pred(event(event)).
% event(E): è avvenuto E
% MODO: predicato che resta in attesa di eventi implementato con readln

event(avvio_gara) :-
	write('\n*************  ENTER per evento avvio gara: '),
	readln(_).

%=============================================================================== DECISIONI

pred(decidi(state,decisione)).
% decidi(S,D):   prendi la decisione D nello stato S corrente
% MODO(++,--) det     (nondet in caso di un agente �erratico�)
pred(do_action(action,state,state)).
% do_action(A,S1,S2):  esegui A nello stato S1 con transizione a S2.
% MODO(++,++,--) det   (nondet con azioni con effetto non deterministico)
pred(pianifica(decisione,state,state,list(action),number)).
% pianifica(Dec,S0,S1,Piano,Costo): Piano = lista di azioni che attua la
% decisone Dec con costo Costo.
% MODO (++,--,--)  nondet
pred(tratta_decisione_impossibile(decisione,state,state)).
% tratta_decisione_impossibile(D,S1,S2): S2 nuovo stato di conscenza,
% sapendo che D è impossibile nello stato S1.    
% MODO (++,++,--) det
pred(stato_interruzione(interrupt, state)).

%------------------------------------------------------------------------------- implementazione dei predicati aperti del decisore
decidi(ferma, do([schierati])) :-
       event(avvio_gara).
decidi(in_gara, gareggia).
decidi(interruzione(punto_occupato(G,p(S,T))), gareggia) :-
	impara(G,p(S,T)),
	maplist(write, ["\n\nMacchina non può andare nel punto(", S, ",", T, ") poichè è presente un avversario al giro ", G]).
decidi(UNKNOWN,_) :-
	%  Per le interruzioni non previste abortisce
	writeln('\n\nInterruzione non prevista':UNKNOWN), abort.

%=============================================================================== PIANIFICAZIONE, eccezioni e decisione impossibile

pianifica(gareggia,_,in_gara,Plan,Cost) :-
	%  in questo caso basta usare piano/3
	piano(gareggia, Plan, Cost).

stato_interruzione(Interr, StatoInterr) :- !,
	get_explanation(Interr, Expl),
	is_interrupt_expl(Expl)->
	StatoInterr = interruzione(Expl)
	;
	StatoInterr = unknown(Interr).

local_pred(is_interrupt_expl(interrupt_expl)).
% serve per filtrare le interrupt_expl previste
is_interrupt_expl(punto_occupato(_,_)).

tratta_decisione_impossibile(_,_,_) :-
	writeln('\n\nQuacosa non va, non sono previste decisioni impossibili'),
	fail.

%=============================================================================== AZIONI
% Azioni con transizione di stato
% Usano esecuzione che implementa l'esecuzione nel mondo virtuale

do_action(Azione,S1,S2) :-
	transizione(Azione,S1,S2),
	esecuzione(Azione).
transizione(schierati,ferma,in_gara).
transizione(taglia_traguardo,in_gara,ferma).
transizione(guida(_,_),in_gara,in_gara).
transizione(effettua_pitstop,in_gara,in_gara).







