:- multifile(skipped/1).

pred(read_command).
%   Esegue readln(Risp); l'esecuzione si ferma in attesa di un input
%   da parte dell'utente che, di norma, preme ENTER per far proseguire;
%   ma l'utente pu� anche scegliere di abortire con a, di passare in
%   trace Prolog con t, o di disattivare trace con n
%
pred(read_command(list(atom))).
%  read_command([P1,P2,--,Pn]) scrive P1P2...,Pn come prompt e
%  esegue read_command
%
pred(traccia_debug(list(atom))).
%  traccia_debug([P1,..,Pn])  scrive:
%
%  traccia_debug:
%     P1P2...Pn
%  fine_traccia_debug:
%
%  e con read_command aspetta una risposta dall'utente

:- dynamic(silent/0).

nodebugtrace :-
	assert(silent).
debugtrace :-
	silent, !; assert(silent).

traccia_debug(L) :-
	silent ->
	true
	;
	maplist(write,['\ntraccia debug:\n   '|L]),
	read_command(['\nfine traccia debug: ']).

read_command :-
	readln(R),
	(   R=[a|_] -> abort
	;   R=[t|_] -> trace
	;   R=[n|_] -> notrace
	;   true).

read_command(Prompt) :-
	maplist(write, Prompt),
	write(' '),
	read_command.

sCRIVI_PER_DEBUG([]).
sCRIVI_PER_DEBUG([E|L]) :-
	write(E),
	sCRIVI_PER_DEBUG(L).