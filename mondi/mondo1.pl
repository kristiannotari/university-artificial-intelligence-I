:- multifile([type/1, pred/1, local_pred/1, open_pred/1, skipped/1, user_unit/1]).

user_unit(mondo1).
%  Un mondo con il tracciato del mugello

type([1,2]:rettilineo).
type([san_donato,luco,poggio_secco]:curva).
type([interna,centrale,esterna]:traiettoria).

% cambio(T1,T2) :  è possibile un cambio di traiettoria da T1 a T2 solo se:
%	- T1 o T2 sono "centrale" (ci si muove solo di una traiettoria per sezione)
%	- T1 = T2
cambio(T1,T2) :-
	(
		T1 = centrale;
		T2 = centrale;
		T1 = T2
	).

tracciato([
	san_donato,
	luco,
	1,
	poggio_secco,
	2
	]).

pitlane_in(p(2, interna)).
pitlane_out(p(2, interna)).

avversario(p(san_donato, interna)).



