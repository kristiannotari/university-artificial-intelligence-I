:- reconsult('rrs/util').
:- multifile([type/1, pred/1, local_pred/1, open_pred/1, skipped/1, user_unit/1]).

user_unit(conoscenza_macchina).
% Rudimentale gestione della conoscenza e apprendimento
% sa(G,p(S,T)) significa sapere che c'è un avversario in p(S,T) al giro G.
% Quindi non posso pianificare di passare da lì.

:- dynamic(sa/1).
:- dynamic(presume/1).

pred(presume(fluent)).
%  presume(F) : presume che F sia vero
%  MODO (?) nondet

pred(sa(fluent)).
% sa(F1): sa che F è vero
% MODO (?,?) nondet

pred(crede(fluent)).
% crede(F): presume o sa che F è vero
% MODO (?) nondet

pred(impara(fluent,fluent)).
% impara(F1,F2): modifica la conoscenza:
% ora sa che F1 con F2 è vera
% MODO (++,++) det

pred(conoscenze(list(fluent),list(fluent))).
%  conoscenze(SA,PR) :  SA lista flunti che l'agente
%  sa esser veri, PR quelli che presume veri

pred(sa_avversario(number,punto)).
% sa_avversario(G,p(S,T)) vero se sono vere:
% sa(giro(G)) e sa(p(S,T))

clear_credenze :-
	retractall(presume(_)),
	retractall(sa(_)).
clear_credenze.

crede(F) :-
	sa(F); presume(F).

impara(G,p(S,T)) :-
	clear_credenze,
	assert(sa(p(S,T))),
	assert(sa(giro(G))).

sa_avversario(G,p(S,T)) :-
	sa(giro(G)),
	sa(p(S,T)).

conoscenze(SA, PR) :-
	(   setof(X,presume(X),PR), ! ; PR=[]),
	(   setof(X,sa(X),SA),!; SA=[]).

