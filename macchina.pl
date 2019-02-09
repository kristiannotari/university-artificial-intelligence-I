:- consult(mondo_macchina).
:- consult(pianificatore_macchina).
:- consult('rrs/decisore').
:- consult('rrs/util').

:- multifile([type/1, pred/1, local_pred/1, open_pred/1, skipped/1, user_unit/1]).

:- maplist(writeln,
['*****************************************************',
 '   Macchina con conoscenza completa; macchina_help',
 '*****************************************************']).

aspirapolvere_help :- maplist(writeln,
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

start :- decisore(attesa).
%  Avvia la macchina

%====================  GLI STATI INTERNI DELLA MACCHINA

type([ferma, in_gara, incidentata]:state).
%  chiudo il tipo state; la macchina attende di essere fatta partire
%  in stato ferm ed procede a nel circuito nello stato in_gara;
%  quando ha finito il giro, ritorna in ferma;
%  se ci sono avversari in tutte e 3 le traiettorie di una sezione del circuito
%  provoca un incidente;
type([esegui_giro]:decisione_complessa).
%  chiudo decisione_complessa con l'unica decisione esegui_giro
%  (fare un giro del circuito senza scontrare altre macchine)
type([avvio_gara]:event).
%  evento scatenato dalla partenza della gara
local_pred(event(event)).
%  event(E) :  è avvenuto E
%  MODO:   predicato che resta in attesa di eventi
%  implementato con readln


pred(decidi(state,decisione)).
%   decidi(S,D) :   prendi la decisione D nello stato S corrente
%   MODO(++,--)   det     (nondet in caso di un agente �erratico�)
pred(do_action(action,state,state)).
%  do_action(A,S1,S2)  :  esegui A nello stato S1 con transizione a S2.
%  MODO(++,++,--) det   (nondet con azioni con effetto non deterministico)
pred(pianifica(decisione,list(action), number)).
%  pianifica(Dec, Piano, Costo) : Piano = lista di azioni che attua la
%  decisone Dec con costo Cost.    MODO (++,--,--)  nondet
pred(tratta_decisione_impossibile(decisione,state,state)).
%  tratta_decisione_impossibile(D,S1,S2)  :   S2 nuovo stato di conscenza,
%   sapendo che D � impossibile nello stato S1.    MODO (++,++,--) det


%======================   IMPLEMENTAZIONE DEI PREDICATI APERTI
%                         DI decisore

%=========================== DECISIONI

decidi(attesa, do([sveglia])) :-
       event(avvio_pulizia).
decidi(pulizia, do([dormi])) :-
       not((sporco(_,Q), Q > 0)), !.
decidi(pulizia, esegui_pulizia).

event(avvio_pulizia) :-
      write('\n*************  ENTER per evento avvio pulizia : '),
      readln(_).



%=====================     PIANIFICAZIONE e decisione impossibile

pianifica(esegui_pulizia, Plan, Cost) :-
	%  in questo caso basta usare piano/3
	piano(esegui_pulizia, Plan, Cost).

tratta_decisione_impossibile(_,_,_) :-
	writeln('Quacosa non va, non sono previste decisioni impossibili'),
	fail.


%====================   LE AZIONI con transizione di stato
%                       Usano esecuzione che implementa l'esecuzione
%                       nel mondo virtuale

do_action(Azione,S1,S2) :-
	transizione(Azione,S1,S2),
	esecuzione(Azione).
transizione(sveglia,attesa,pulizia).
transizione(dormi, pulizia, attesa).
transizione(va(_S), pulizia, pulizia).
transizione(versa_sporco, pulizia, pulizia).
transizione(pulisci(_Q), pulizia, pulizia).








