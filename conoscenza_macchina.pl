:- reconsult('rrs/util').
:- multifile([type/1, pred/1, local_pred/1, open_pred/1, skipped/1, user_unit/1]).

%===============================================================================  DEFINIZIONE CONOSCENZA

user_unit(conoscenza_macchina).
% Rudimentale gestione della conoscenza e apprendimento.
% La macchina presume o sa la posizione degli avversari nel tracciato.

clear_credenze :-
	retractall(presume(_)),
	retractall(sa(_)).

:- dynamic(sa/1).
:- dynamic(presume/1).

pred(presume(fluent)).
% presume(F): presume che F sia vero
% MODO (?) nondet

pred(sa(fluent)).
% sa(F): sa che F è vero
% MODO (?) nondet

pred(crede(fluent)).
% crede(F): presume o sa che F è vero
% MODO (?) nondet

pred(impara(fluent)).
% impara(F): modifica la conoscenza di F:
% 	ora sa che F é vera
% MODO (++) det

pred(conoscenze(list(fluent),list(fluent))).
%  conoscenze(SA,PR) :  SA lista flunti che l'agente
%  sa esser veri, PR quelli che presume veri

%===============================================================================  IMPLEMENTAZIONE CONOSCENZA

crede(F) :-
	sa(F); presume(F).

impara(avversario(Nome,p(S,T))) :-
	retractall(presume(avversario(Nome,_))),
	retractall(sa(avversario(Nome,_))),
	assert(sa(avversario(Nome,p(S,T)))).

conoscenze(SA, PR) :-
	(   setof(X,presume(X),PR), ! ; PR=[]),
	(   setof(X,sa(X),SA),!; SA=[]).

