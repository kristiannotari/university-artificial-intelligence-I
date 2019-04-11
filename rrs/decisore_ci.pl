:- use_module('../lib/pl_types').
:- use_module(util).
:- multifile([type/1, pred/1, local_pred/1, open_pred/1, skipped/1]).

user_unit(decisore_ci).
%  Decisore con conoscenza incompleta. Le differenze principali
%  rispetto ai decisori considerati in precedenza sono:

%  A) In fase di decisione e pinificazione l'agente non ha conoscenza
%  completa, per cui decide e pianifica in base a quanto presume sul
%  mondo; le assunzioni che fa sono dettate dall'esperienza passata o da
%  conoscenze presunte, che chiameremo "credenze".
%  VEDERE conoscenza_aspirapolvere.pl

% B) In fase di esecuzione di un piano, l'agente opera nel mondo reale
%  e può trovare situazioni diverse da quelle presunte; in tal caso
%  deve rivedere decisioni e piani. Il meccanismo tecnico attraverso
%  il quale viene interrotto un comportamento pianificato e si passa a
%  nuove decisioni è quello delle eccezioni (usando throw e catch
%  prolog).  Le eccezioni vengono lanciate in esecuzione nel mondo
%  reale, VEDERE  mondo_aspirapolvere.pl

% C) Sempre in fase di esecuzione, man mano che l'agente scopre come
%  stanno realmente le cose nel mondo, modifica le sue "credenze",
%  memorizzando quanto scoperto; diremo che l'agente IMPARA.
%  Tecnicamente, le "credenze" sono codificate con predicati dinamici e
%  uno o più predicati di apprendimento governano la revisione delle
%  "credenze". In questo esempio useremo un unico predicato di
%  apprendimento impara_in/1.
%  VEDERE conoscenza_aspirapolvere.pl
%

type(open:state).
% stato interno, può accumulare conoscenza ed esperienza passata
type(open:decisione_complessa).
% decisioni che richiedono pianificazione
type(open:action).
% azioni base
type(open:interrupt_expl).
% spiegazioni interruzione azioni
type([interr(interrupt_expl, state, action, list(action))]:action_interrupt).
% interr(C, S, A, AR): nello stato S l'azione A è stata interrotta per
% la causa C e AR sono le azioni rimanenti del piano interrotto
type([interr(action_interrupt, state, decisione_complessa)]:interrupt).
% interr(Interr, St, Dec) : nello stato S l'attuazione della decisione
% Dec è stata interrotta nel modo indicato in Interr, dove Interr è la
% action_interrupt lanciata a livello di azioni
type([do(list(action)),{decisione_complessa}]:decisione).
% decisioni

%=========================    PREDICATI APERTI DA IMPLEMENTARE

open_pred(decidi(state,decisione)).
%   decidi(S,D) :   prendi la decisione D nello stato S corrente
%   MODO(++,--)   det
open_pred(do_action(action,state,state)).
%  do_action(A,S1,S2)  :  esegui A nello stato S1 con transizione a S2.
%  MODO(++,++,--) det
open_pred(pianifica(decisione,state,
		    state,list(action), number)).
%  pianifica(D, S1, S2, Piano, Cost) : Piano = lista di azioni che attua
%  la decisione D a partire dallo stato S2 con costo Cost.
%  MODO (++,++,--,--,--)
open_pred(tratta_decisione_impossibile(decisione,state,state)).
%  tratta_decisione_impossibile(D,S1,S2) : S2 nuovo stato di conscenza,
%  sapendo che D è impossibile nello stato S1. MODO (++,++,--) det

open_pred(stato_interruzione(interrupt, state)).
%  stato_interruzione(Exc,StExc): StExc è uni stato interno
%  dell'agente che tien conto dell'eccezione Exc e contiene le
%  conoscenze necessarie a prendere una nuova decisione in base
%  alla eccezione riscontrata.
%  MODO (++,++,--) det

%=============================== PREDICATI AGENTE DECISORE

pred(decisore(state)).
% decisore(S): produce il comportamento dell’agente a partire dallo stato
% S. PROCESSO
local_pred(attua(decisione, state, state)).
%   attua(D,S1,S2) : attua la decisione D nello stato S1;   alla fine si avrà lo stato S2
%   MODO  (++,++,--) det
local_pred(do(list(action), state, state)).
%   do(ActionList,S1,S2) : esegue sequenzialmente ActionList, con passaggio da S1 a S2
%   MODO (++,++,--) det

% ============================ ESTRAZIONE INFORMAZINI INTERRUPT
%   i termini di tipo interrupt contengono le seguenti informazioni
%   NOTA: il tipo interrupt qui consoderato è un esempio
%   PERSONSLIZZABILE

pred(get_explanation(interrupt, interrupt_expl)).
% get_explanation(Int, Expl) : Expl spiega cosa ha causato
% l'azione interrotta.   MODO (++,--) det
pred(get_action(interrupt, action)).
% get_action(Interr,A) : A è l'azione interrotta di Interr
%  MODO (++,--) det
pred(get_action_state(interrupt, state)).
% get_action_state(Interr,S) : S è lo stato in cui è stata interrotta
% l'azione.  MODO (++,--) det
pred(get_remaining_actions(interrupt, list(action))).
% get_remaining_actions(Interr,ActList) : ActList sono le eventuali
% azioni del piano da eseguire dopo l'azione interrotta
% MODO (++,--) det
pred(get_decision_state(interrupt, state)).
% get_decision_state(Interr,S) : S è lo stato in cui è stata avviata
% la attuazione della decisione interrotta.  MODO (++,--) det
pred(get_decision(interrupt, decisione)).
% get_decision(Interr,D) : D è la decisione la cui attuazione
% è stata interrotta. MODO (++,--) det


% ===================================== IMPLEMENTAZIONE
% ==========



decisore(Stato) :-
	decidi(Stato, Decisione),
	traccia_debug(['STATO ', Stato, '\n  DECISIONE: ', Decisione]),
	catch( attua(Decisione, Stato, NuovoStato),
	       Interr,
	       (   stato_interruzione(Interr, NuovoStato),
		   traccia_debug(['INTERRUZIONE: ', Interr,
				  '\n   NUOVO STATO: ', NuovoStato]))
	       ),
	decisore(NuovoStato).

attua(do(ActionList), S1, S2) :- !,
	do(ActionList, S1, S2).
attua(Decisione,S1,S2) :-
	conoscenze(SO,PR),
	traccia_debug(['STATO ', S1, ' SO: ', SO,
		       '\n  PRESUMO: ', PR,
		       '\n  PIANIFICO: ', Decisione]),
	pianifica(Decisione, S1, SP, ActionList, Cost) ->
	traccia_debug(['Piano con costo ', Cost, ' e nuovo stato ', SP, ':\n   ',
		     ActionList]),
	catch(do(ActionList,SP,S2),
	      ActInterr,
	      throw(interr(ActInterr, S1, Decisione)))
	;
	tratta_decisione_impossibile(Decisione,S1,S2).

do([],S,S).
do([A|NextActions],S1,S_end) :-
       catch(do_action(A,S1,S2),
	      Expl,
	      throw(interr(Expl, S1, A, NextActions))),
	do(NextActions, S2, S_end).



% ============= getters: indicano le informazioni estraibili da una
% eccezione


get_explanation(interr(interr(Expl,_ActionState,_Action,_Remaining),_DecisionState,_Decision), Expl).
get_action(interr(interr(_Expl,_ActionState,Action,_Remaining),_DecisionState,_Decision), Action).
get_action_state(interr(interr(_Expl,ActionState,_Action,_Remaining),_DecisionState,_Decision), ActionState).
get_remaining_actions(interr(interr(_Expl,_ActionState,_Action,Remaining),_DecisionState,_Decision), Remaining).
get_decision_state(interr(interr(_Expl,_ActionState,_Action,_Remaining),DecisionState,_Decision), DecisionState).
get_decision(interr(interr(_Expl,_ActionState,_Action,_Remaining),_DecisionState,Decision), Decision).
