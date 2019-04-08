:- consult(mondo_macchina).
:- consult(pianificatore_macchina).
:- consult('rrs/decisore').
:- consult('rrs/util').

:- multifile([type/1, pred/1, local_pred/1, open_pred/1, skipped/1, user_unit/1]).

:- maplist(writeln,
['*****************************************************',
 '   Macchina con conoscenza completa; macchina_help',
 '*****************************************************']).

macchina_help :- maplist(writeln,
['HELP',
 '?- start. per avviare la macchina',
 ' Traccia di debug attiva per default,',
 ' nodebugtrace per escluderla,  debugtrace per riattivarla\n']).

user_unit(macchina).
%  unità che governa la "vita" della macchina usando
%  l'RRS decisore  (in 'rrs/decisore.pl').
%  Usa (importa) i tipi e i predicati definiti in mondo_macchina
%
% import_type(mondo_macchina:action).
%    action, le azioni del mondo macchina+circuito da pulire
% import_type(mondo_macchina:luogo).
%    i luoghi, stanze e balcone
% import_pred(mondo_macchina:sporco(luogo,number)).
%    lo sporco nei  luaoghi
% import_pred(mondo_aspirapolvere:esecuzione(action)).
%     il mondo delle stanze da puire ed esecuzione
%     effettiva delle azioni dell'agente nel mondo
%
% Usa il predicato
%   piano(decisione_complessa, list(action), number)).
%   di pianificatore_aspirapolvere

start :- decisore(ferma).
%  Avvia la macchina

%====================  GLI STATI INTERNI DELLA MACCHINA

type([ferma,in_gara,incidentata]:state).
% Chiudo il tipo state; la macchina attende di essere fatta partire in stato
% ferma e procede nel circuito nello stato in_gara.
% Quando ha finito il giro, ritorna in ferma.
% Se non può spostarsi in un punto successivo, causa un'incidente.
type([gareggia]:decisione_complessa).
% Chiudo decisione_complessa con l'unica decisione gareggia
% (fare N giri del circuito senza scontrare altre macchine)
type([avvio_gara]:event).
% Evento scatenato dalla partenza della gara
local_pred(event(event)).
% event(E): è avvenuto E
% MODO: predicato che resta in attesa di eventi implementato con readln

pred(decidi(state,decisione)).
% decidi(S,D):   prendi la decisione D nello stato S corrente
% MODO(++,--) det     (nondet in caso di un agente �erratico�)
pred(do_action(action,state,state)).
% do_action(A,S1,S2):  esegui A nello stato S1 con transizione a S2.
% MODO(++,++,--) det   (nondet con azioni con effetto non deterministico)
pred(pianifica(decisione,list(action), number)).
% pianifica(Dec, Piano, Costo): Piano = lista di azioni che attua la
% decisone Dec con costo Cost.
% MODO (++,--,--)  nondet
pred(tratta_decisione_impossibile(decisione,state,state)).
% tratta_decisione_impossibile(D,S1,S2): S2 nuovo stato di conscenza,
% sapendo che D è impossibile nello stato S1.    
% MODO (++,++,--) det


% IMPLEMENTAZIONE DEI PREDICATI APERTI DI decisore
%=============================================================================== DECISIONI

decidi(ferma, do([schierati])) :-
       event(avvio_gara).
decidi(in_gara, gareggia).

event(avvio_gara) :-
      write('\n*************  ENTER per evento avvio gara: '),
      readln(_).

%=============================================================================== PIANIFICAZIONE e decisione impossibile

pianifica(gareggia, Plan, Cost) :-
	%  in questo caso basta usare piano/3
	piano(gareggia, Plan, Cost).

tratta_decisione_impossibile(_,_,_) :-
	writeln('Quacosa non va, non sono previste decisioni impossibili'),
	fail.

%=============================================================================== AZIONI
% Azioni con transizione di stato
% Usano esecuzione che implementa l'esecuzione nel mondo virtuale

do_action(Azione,S1,S2) :-
	transizione(Azione,S1,S2),
	esecuzione(Azione).
transizione(schierati,ferma,in_gara).
transizione(taglia_traguardo,in_gara,ferma).
transizione(guida(_),in_gara,in_gara).
transizione(effettua_pitstop,in_gara,in_gara).







