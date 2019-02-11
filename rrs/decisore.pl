:- use_module('../lib/pl_types').
:- consult(util).

:- maplist(writeln,[
'******************************************************',
'     DECISORE con pianificazione',
'******************************************************'
	   ] ).


user_unit(decisore).
%  Unità che contiene una bozza primitiva di agente decisore e
%  ne fornisce il comportamento.
%  Usa come tipi aperti state (gli stati interni dell'agente),
%  action (le azioni dell'agente), decisione_complessa (le decisoni
%  dell'agente che richiedono pianificazione)
%  Il tipo delle decisioni dell'agente comprende:
%  -  do(lista azioni)  : decisione di eseguire la lista di azioni
%  -  DecisioneComplessa :  una decisione che richiede pianificazione
%  Il predicato decisore(Stato) avvia e realizza il comportanento
%  dell'agente, pur di implememtare i predicati aperti:
%
%  -  decidi(St,Dec)   prende una decisione nello stato St
%
%  -  do_action(Act,S1,S2) :  esegue nel mondo esterno Act e passa
%     da stato interno S1 a stato interno S2
%
%  - pianifica(Dec,St,P,C) : calcola un piano P e relativo costo C per
%  eseguire la decisione Dec
%
%  - tratta_decisione_impossibile(Dec,St1,St2) :  Dec non ha piano
%  esecutivo nello stato interno St1:  St2 accumula la conoscenza
%  della impossibilità di Dec

type(open:state).
% stato interno, può accumulare conoscenza ed esperienza passata
type(open:decisione_complessa).
% decisioni che richiedono pianificazione
type(open:action).
% azioni base
type([do(list(action)),{decisione_complessa}]:decisione).
% decisioni
open_pred(decidi(state,decisione)).
%   decidi(S,D) :   prendi la decisione D nello stato S corrente
%   MODO(++,--)   det     (nondet in caso di un agente �erratico�)
open_pred(do_action(action,state,state)).
%  do_action(A,S1,S2)  :  esegui A nello stato S1 con transizione a S2.
%  MODO(++,++,--) det   (nondet con azioni con effetto non deterministico)
open_pred(pianifica(decisione,state,list(action),number)).
%  pianifica(D, S, Piano, Costo) : Piano = lista di azioni che attua la
%  decisone D.    MODO (++,++,--,--)  nondet
open_pred(tratta_decisione_impossibile(decisione,state,state)).
%  tratta_decisione_impossibile(D,S1,S2)  :   S2 nuovo stato di conscenza,
%   sapendo che D è impossibile nello stato S1.    MODO (++,++,--) det


pred(decisore(state)).
% decisore(S): produce il comportamento dell'agente a partire dallo stato
% S. PROCESSO
local_pred(attua(decisione, state, state)).
%   attua(D,S1,S2) : attua la decisione D nello stato S1;   alla fine si avrà lo stato S2
%   MODO  (++,++,--) det   (nondet per agenti erratici o azioni con effetto non deterministico)
local_pred(do(list(action), state, state)).
%   do(ActionList,S1,S2) : esegue sequenzialmente ActionList, con passaggio da S1 a S2
%   MODO (++,++,--det)   (nondet con azioni con effetto non deterministico)

%=====================================


decisore(Stato) :-
	decidi(Stato, Decisione),
	traccia_debug(['STATO ', Stato, '\n  DECISIONE: ', Decisione]),
	attua(Decisione, Stato, NuovoStato),
	decisore(NuovoStato).

attua(do(ActionList), S1, S2) :- !,
	do(ActionList, S1, S2).
attua(Decisione,S1,S2) :-
	traccia_debug(['STATO ', S1, ', PIANIFICO: ', Decisione]),
	pianifica(Decisione, ActionList, Cost) ->
	traccia_debug(['Piano con costo ', Cost, ':\n   ',
		       ActionList]),
	do(ActionList, S1, S2)
	;
	tratta_decisione_impossibile(Decisione,S1,S2),
	traccia_debug([Decisione, ' IMPOSSIBILE, nuovo stato : ', S2]).

do([],S,S).
do([A|AA],S1,S2) :-
	do_action(A,S1,NextS),
	do(AA, NextS, S2).

%================






