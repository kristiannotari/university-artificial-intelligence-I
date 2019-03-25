:- multifile([type/1, pred/1, local_pred/1, open_pred/1, skipped/1, user_unit/1]).

user_unit(mondo1).
% Un mondo con il tracciato del Mugello

type([san_donato,luco,poggio_secco,1,2]:sezione).
type([interna,centrale,esterna]:traiettoria).

%=============================================================================== MONDO DI GARA
% Mondo di gara descritto attraverso la definizione di:
%	- numero giri
%	- tracciato
%	- curve
%	- rettilinei
%	- pitlane in e out
%	- traiettorie
%	- avversari
% "Minima" usura massima = 5 per 1 giro

giri(2).

tracciato([
	san_donato,
	luco,
	1,
	poggio_secco,
	2
]).

curva(san_donato).
curva(luco).
curva(poggio_secco).

rettilineo(1).
rettilineo(2).

pitlane_in(p(2,interna)).
pitlane_out(p(san_donato,interna)).

traiettoria(esterna).
traiettoria(centrale).
traiettoria(interna).

avversario(hamilton, p(san_donato,esterna)).
avversario(vettel, p(luco,esterna)).

%=============================================================================== REGOLE DI GARA

% cambio(T1,T2): è possibile un cambio di traiettoria da T1 a T2 solo se:
%	- T1 o T2 sono "centrale" (ci si muove solo di una traiettoria per sezione)
%	- T1 = T2
cambio(T1,T2) :-
	T1 = centrale;
	T2 = centrale;
	T1 = T2.

% calc_usura(S,T,Q,Q1):
%	- S è una curva (T=interna->Q1=Q+1, T=centrale->Q1=Q+2, T=esterna->Q1=Q+3)
%	- S è un rettilineo (T=interna->Q1=Q+1, T=centrale->Q1=Q+1, T=esterna->Q1=Q+1)
calc_usura(S,interna,Q,Q1) :-
	curva(S),
	Q1 is Q + 1.
calc_usura(S,centrale,Q,Q1) :-
	curva(S),
	Q1 is Q + 2.
calc_usura(S,esterna,Q,Q1) :-
	curva(S),
	Q1 is Q + 3.
calc_usura(S,_,Q,Q1) :-
	rettilineo(S),
	Q1 is Q + 1.