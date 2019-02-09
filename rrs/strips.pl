:- use_module('../lib/pl_types').
:- multifile([type/1, pred/1, local_pred/1, open_pred/1, skipped/1]).

:- maplist(writeln, [
'\n*************************************************************',
' CARICATO STRIPS: strips_help',
'***************************************************************']).

strips_help :- maplist(write, ['\nstrips fornisce:',
'\n1) Il tipo degli stati di pianificazione alla strips, implemntati come',
'\n   ordset(fluent),  dove il tipo fluent è lasciato aperto',
'\n2) p_do(Azione, S1,S2, C):   esecuzione ipotetica nello spazio degli',
'\n   stati di pianificazione, utilizzta da forward_planner',
'\n\nRichiede di implementare:',
'\n1) add_del(?Azione, ++StatoPianificazione, ?Add, ?Del, ?Cost):',
'\n   la add_del come nel modello di azioni strips'
]).

user_unit(strips).
% strips fornisce:
%
% - il tipo degli stati di pianificazione alla strips, implemntati come
% ordset(fluent)
%
% - p_do(Azione, S1,S2, C): esecuzione ipotetica nello spazio degli
% stati di pianificazione, utilizzta da forward_planner',
%
% Richiede di implementare:
% add_del(?Azione, ++StatoPianificazione, ?Add, ?Del, ?Cost):
%    la add_del come nel modello di azioni strips

type([{list(fluent)}]: p_node).
%   stati come insiemi (ordinati) di fluenti

type(open:fluent).
%  i fluenti

pred(strips_help).
%  piccolo help

pred(p_do(action, p_node, p_node, number)).
%  p_do(Act, S1, S2, C) : Act è eseguibile nello stato S1 e
%  manda S1 in S2 con costo C
%  MODO (?,++,?,?) nondet: vedi forward planner
%  MODO (--,++,++,--) semidet: vedi forward planner

open_pred(add_del(action, p_node, list(fluent), list(fluent), number)).
%  add_del(A, S, Add, Del, Cost) rappresenta l'effetto di una
%  azione A come segue:
%       1)   A è possibile nello stato S
%       2)   La sua esecuzione rende veri i fluenti in Add
%            e falsi quelli in Del
%  MODO:  (?, ++, ?, ?) nondet.
%  MODO:  (--,++,++,--) semidet

local_pred(add(list(fluent), p_node, p_node)).
%  add(L, S1, S2) :  S2 = S1 Union L
%  MODO  (++,++,--) det
%
local_pred(remove(list(fluent), p_node, p_node)).
%  add(L, S1, S2) :  S2 = S1 \ L
%  MODO  (++,++,--) det

% per saltare il controllo di tipo su questi predicati
skipped(ord_add_element/3).
skipped(ord_del_element/3).

p_do(Act, S1, S2, Cost) :-
	add_del(Act, S1, Add, Del, Cost),
	remove(Del, S1, S),
	add(Add,S,S2).


remove([], S1, S1).
remove([F|L], S1, S2) :-
	ord_del_element(S1,F,S),
	remove(L,S,S2).

add([], S1, S1).
add([F|L], S1, S2) :-
	ord_add_element(S1,F,S),
	add(L,S,S2).

















