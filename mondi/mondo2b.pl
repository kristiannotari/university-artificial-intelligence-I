:- multifile([type/1, pred/1, local_pred/1, open_pred/1, skipped/1, user_unit/1]).

user_unit(mondo2b).
% Un mondo con il tracciato del Mugello e 3 giri

type([
	rettifilo,
	san_donato,
	luco,
	poggio_secco,
	1,
	materassi,
	borgo_san_lorenzo,
	2,
	casanova,
	savelli,
	arrabbiata1,
	arrabbiata2,
	3,
	scarperia,
	palagio,
	4,
	correntaio,
	biondetti1,
	biondetti2,
	5,
	bucine
]:sezione).
type([interna,centrale,esterna]:traiettoria).

%=============================================================================== MONDO DI GARA
% Mondo di gara descritto attraverso la definizione di:
%	- numero giri
%	- tracciato (sez_succ)
%	- curve
%	- rettilinei
%	- pitlane in e out
%	- traiettorie
%	- avversari
% "Minima" usura massima = 5 per 1 giro

giri(3).

sez_succ(rettifilo,san_donato).
sez_succ(san_donato,luco).
sez_succ(luco,poggio_secco).
sez_succ(poggio_secco,1).
sez_succ(1,materassi).
sez_succ(materassi,borgo_san_lorenzo).
sez_succ(borgo_san_lorenzo,2).
sez_succ(2,casanova).
sez_succ(casanova,savelli).
sez_succ(savelli,arrabbiata1).
sez_succ(arrabbiata1,arrabbiata2).
sez_succ(arrabbiata2,3).
sez_succ(3,scarperia).
sez_succ(scarperia,palagio).
sez_succ(palagio,4).
sez_succ(4,correntaio).
sez_succ(correntaio,biondetti1).
sez_succ(biondetti1,biondetti2).
sez_succ(biondetti2,5).
sez_succ(5,bucine).
sez_succ(bucine,rettifilo).

griglia(rettifilo,esterna).
traguardo(rettifilo).

curva(san_donato).
curva(luco).
curva(poggio_secco).
curva(materassi).
curva(borgo_san_lorenzo).
curva(casanova).
curva(savelli).
curva(arrabbiata1).
curva(arrabbiata2).
curva(scarperia).
curva(palagio).
curva(correntaio).
curva(biondetti1).
curva(biondetti2).
curva(bucine).

rettilineo(rettifilo).
rettilineo(1).
rettilineo(2).
rettilineo(3).
rettilineo(4).
rettilineo(5).

traiettoria(interna).
traiettoria(centrale).
traiettoria(esterna).

pitlane_costo(3).
pitlane_in(bucine,esterna).
pitlane_out(san_donato,interna).

avversario(hamilton,san_donato,esterna).
% avversario(vettel,casanova,interna).

%=============================================================================== COSTI DI GARA

costo(S,interna,1) :-
	curva(S), !.
costo(S,centrale,2) :-
	curva(S), !.
costo(S,esterna,3) :-
	curva(S), !.
costo(rettifilo,centrale,1) :- !.
costo(1,centrale,1) :- !.
costo(2,esterna,1) :- !.
costo(3,esterna,1) :- !.
costo(4,centrale,1) :- !.
costo(5,centrale,1) :- !.
costo(S,T,2) :-
	traiettoria(T),
	rettilineo(S), !.