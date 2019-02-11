:- multifile([type/1, pred/1, local_pred/1, open_pred/1, skipped/1, user_unit/1]).

user_unit(mondo1).
%  Un mondo con il tracciato del mugello

type([1,2]:rettilineo).
type([san_donato,luco,poggio_secco]:curva).
type({curva,rettilineo}:sezione).
type([interna,centrale,esterna]:traiettoria).

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
pitlane_in(p(2, interna)).
pitlane_out(p(2, interna)).

avversario(p(san_donato, interna)).

% cambio(T1,T2): è possibile un cambio di traiettoria da T1 a T2 solo se:
%	- T1 o T2 sono "centrale" (ci si muove solo di una traiettoria per sezione)
%	- T1 = T2
cambio(T1,T2) :-
	(
		T1 = centrale;
		T2 = centrale;
		T1 = T2
	).

% calc_usura(S,T,Q,Q1):
%	- S è una curva (T=interna->Q1=Q+1, T=centrale->Q1=Q+2, T=esterna->Q1=Q+3)
%	- S è un rettilineo (T=interna->Q1=Q+1, T=centrale->Q1=Q+1, T=esterna->Q1=Q+1)
calc_usura(S,interna,Q,Q1) :-
	curva(S), !,
	Q1 is Q + 1.
calc_usura(S,centrale,Q,Q1) :-
	curva(S), !,
	Q1 is Q + 2.
calc_usura(S,esterna,Q,Q1) :-
	curva(S), !,
	Q1 is Q + 3.
calc_usura(S,_,Q,Q1) :-
	rettilineo(S), !,
	Q1 is Q + 1.

